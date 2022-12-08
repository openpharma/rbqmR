#' Apply an arbitrary QTL rule to a tibble
#' 
#' This function requires two tibbles and a function, plus arbitrary arguments.
#' One tibble contains observed site level metrics.  The other tibble contains
#' a posterior estimate of the distribution of the metric.  Applying the
#' function should determine (a) whether or not the QTL has been breached (and 
#' at what level - eg action or warning) and (b) which observations in the site
#' level tibble contribute to the breach.
#' 
#' This functions serves both as the common basis on which more specific QTL
#' rules may be evaluated and also as an avenue for the evaluation of study-
#' specific or other custom QTL rules may be evaluated.
#' @param data the tibble containing site-level observed metrics
#' @param posterior the tibble containing the posterior distribution of the
#' metric, usually obtained from a fit Bayes model function.
#' @param f the function that evaluates the QTL.  The first two parameters of
#' `f` should be `data` and `posterior` as defined above.
#' @param statusCol The name of the column in the augmented `data` tibble that
#' defines the status of the corresponding row
#' @param ... Other arguments passed to `f`.
#' @return a list containing at least two elements.  One element is a character
#' string named `status` that reports the status of the QTL rule.  If the QTL
#' has not been breached, the value of this element should be `"OK"`.  Other
#' values will depend on the actions of `f`, but use of a consistent nomenclature
#' is recommended.  Functions provided by the `rbqmR` package itself will return
#' values such as `"WARNING"` or `"ACTION"`.  The second elements is named `data`
#' that contains a copy of the `data` tibble augmented with  `statusCol`.
#' 
#' Other elements of the return value depend on the actions of `f`.
#' @export
evaluateCustomQTL <- function(
                       data,
                       posterior,
                       f,
                       statusCol=Status,
                       ...
                     ) {
  logger::log_debug("Entry")
  # Validate
  if (!is.data.frame(data)) stop("data is not a data.frame")
  if (!is.data.frame(posterior)) stop("posterior is not a data.frame")
  if (is.null(f)) stop("f cannot be NULL")
  if (!is.function(f)) stop("f is not a function")
  argsOfF <- names(formals(f))
  if (argsOfF[1] != "data") stop("First argument of f is not named 'data'")
  if (argsOfF[2] != "posterior") stop("Second argument of f is not named 'posterior'")
  # Execute
  rv <- data %>% f(posterior, ...)
  logger::log_debug("Exit")
  rv
}
#' Compares a scalar statistic derived from the posterior with one or more fixed values
#' 
#' @param data the tibble containing site-level observed metrics (KRIs)
#' @param posterior the tibble containing the posterior distribution of the
#' @param metric the column in `posterior` on which the metric should be based
#' @param stat the summary statistic to be calculated from the values in 
#' `metric`.  Default: `mean`.
#' @param observedMetric the column in `data` containing the site level 
#' observed metrics (KRIs)
#' @param statusCol the name of the column to be added to `data` that will
#' contain the site-level flags comparing the corresponding KRI to the QTL
#' thresholds defined by `lower` and `upper`
#' @param lower a scalar or vector of lower limits, or NULL.  See Usage Notes 
#' below.
#' @param upper a scalar or vector of upper limits, or NULL.  See Usage Notes 
#' below.
#' @return a list of three elements named `status`, `data` and `qtl`.  If
#' `status` is `"OK"`, no breach of the QTL was detected.  Otherwise, `status` 
#' is equal to name of the most serious breach of the QTL as determined by the 
#' values in either `lower` or `upper`.  `qtl` contains the value of the `stat`
#' function when applied to `metric`.
#' @section Usage Notes:
#' Both `lower` and `upper` cannot be NULL.  In what follows, `limit` refers to
#' both `lower` and `upper`.  
#' 
#' If `limit` has names, those names are used.  Otherwise, names are assigned 
#' based on the length of `limit`.  If `length(limit)` is `1`, the single 
#' element is named `"action"`.  If `length(limit)` is `2`, the two elements are
#' named `"warn"` and `"action"`.  (In this case, `limit` is first sorted
#' appropriately.)  If `length(limit)` is `>2`, its elements are named `"1"` to 
#' `"n"` where `n` is `length(limit)`.
#' @examples
#' post <- (berrySummary %>% fitBayesBinomialModel(Subjects, Events))$tab
#` berrySummary %>% 
#` evaluatePointEstimateQTL(
#`   posterior=post, 
#`   metric = p, 
#`   observedMetric=ObservedResponse,
#`   lower=c("warn"=0.1, "action"=0.2),
#`   upper=c("warn"=0.7, "action"=0.9)
#` )
#' @export
evaluatePointEstimateQTL <- function(
                              data,
                              posterior,
                              metric,
                              stat=mean,
                              observedMetric,
                              statusCol=Status,
                              lower=NULL,
                              upper=NULL
                            ) {
  logger::log_debug("Entry")
  logger::log_trace(deparse(match.call()))
  # Validate
  if (is.null(lower) & is.null(upper)) stop("Both lower and upper cannot be NULL")
  if (!is.data.frame(data)) stop("data is not a data.frame")
  data %>% .assertColumnExists({{ observedMetric }})
  data %>% .assertColumnDoesNotExist({{ statusCol }})
  posterior %>% .assertColumnExists({{ metric }})
  if (is.null(stat)) stop("stat cannot be NULL")
  if (!is.function(stat)) stop("stat is not a function")
  # Prepare
  upper <- upper %>% .ensureLimitsAreNamed(decreasing = TRUE)
  lower <- lower %>% .ensureLimitsAreNamed()
  # Execute
  func <- function(data, posterior) {
    rv <- list("status"="OK")
    rv$qtl <- posterior %>% 
                dplyr::summarise(qtl=stat({{ metric }})) %>% 
                dplyr::pull(qtl)
    if (length(lower) > 0) {
      for (i in length(lower):1) {
        x <- names(lower)[i]
        if (rv$qtl < lower[x]) rv$status <- x
      }
    }
    if (length(upper) > 0) {
      for (x in names(upper)) {
        if (rv$qtl > upper[x]) rv$status <- x
      }
    }
    rv$data <- data %>% 
                .addStatusToObservedData(
                  {{ statusCol }}, 
                  {{ observedMetric }}, 
                  lower, 
                  upper
                )
    rv
  }
  tryCatch({
      rv <- data %>% evaluateCustomQTL(posterior, func)
    },
    error=function(e) {
      print(e$message)
      rv <- list(
              "data"=NA,
              "status"="ERROR",
              "message"=e$message,
              "qtl"=NA
            )
    }
  )
  logger::log_debug("Exit")
  rv
}

#' Evaluates a QTL based on prob(study-level metric lies within a range)
#' 
#' Evaluates the probability that a study-level metric lies within a given range
#' based in the posterior density provided by a call to a fit Bayes XXXX Model
#' function.  Action and warning limits are determined by parameters `range` and
#' `probs`.  That is, for a single range for the metric, various limits are 
#' defined, each based on an element of `probs`.  Site-level KRIs are flagged
#' based on the values in `lower` and `upper`.
#' 
#' For more details, see Usage Notes below.
#' @param data the tibble containing site-level observed metrics (KRIs)
#' @param posterior the tibble containing the posterior distribution of the, 
#' metric, usually obtained from a fit Bayes model function.
#' @param metric the column in `posterior` on which the QTL should be based
#' @param observedMetric the column in `data` containing the site level 
#' observed metrics (KRIs)
#' @param statusCol the name of the column to be added to `data` that will
#' contain the site-level flags comparing the corresponding KRI to the QTL
#' thresholds defined by `lower` and `upper`
#' @param range the range of values of `metric` to be used when calculating
#' `qtl` = Prob(`metric` in `range`).
#' @param probs the thresholds for Prob(`metric` in `range`) that define the 
#' action and warning limits for the QTL
#' @param lower a scalar or vector of lower limits, or NULL.  See Usage Notes 
#' below.
#' @param upper a scalar or vector of upper limits, or NULL.  See Usage Notes 
#' below.
#' @return a list of three elements named `status`, `data` and `qtl`.  If
#' `status` is `"OK"`, no breach of the QTL was detected.  Otherwise, `status` 
#' is equal to name of the most serious breach of the QTL as determined by the 
#' values in either `lower` or `upper`. `qtl` contains the posterior probability
#' that `metric` lies within `range`. 
#' @section Usage Notes:
#' Either or both `lower` of `upper` can be NULL.  In what follows, `limit` refers to
#' both `lower` and `upper`.  
#' #' If `limit` has names, those names are used.  Otherwise, names are assigned 
#' based on the length of `limit`.  If `length(limit)` is `1`, the single 
#' element is named `"action"`.  If `length(limit)` is `2`, the two elements are
#' named `"warn"` and `"action"`.  (In this case, `limit` is first sorted
#' appropriately.)  If `length(limit)` is `>2`, its elements are named `"1"` to 
#' `"n"` where `n` is `length(limit)`.
#' @export
evaluateProbabilityInRangeQTL <- function(
                                   data,
                                   posterior,
                                   metric,
                                   observedMetric,
                                   statusCol=Status,
                                   range,
                                   probs,
                                   lower=NULL,
                                   upper=NULL
                                 ) {
  logger::log_debug("Entry")
  logger::log_trace(deparse(match.call()))
  # Validate
  if (!is.data.frame(data)) stop("data is not a data.frame")
  data %>% .assertColumnExists({{ observedMetric }})
  data %>% .assertColumnDoesNotExist({{ statusCol }})
  posterior %>% .assertColumnExists({{ metric }})
  if(length(range) != 2) stop("range must be a numeric vector of length 2")
  if (range[2] <= range[1]) stop("values in range must be in strictly increasing order")
  if (any(is.na(range))) stop("range must not include any NA values")
  if (any(is.na(probs))) stop("probs must not include any NA values")
  if (any(probs > 1)) stop("all values of probs must be strictly less than 1")
  if (any(probs < 0)) stop("all values of probs must be strictly greater than 0")
  # Prepare
  lower <- lower %>% .ensureLimitsAreNamed()
  upper <- upper %>% .ensureLimitsAreNamed(decreasing = TRUE)
  probs <- sort(probs %>% .ensureLimitsAreNamed())
  # Execute
  func <- function(data, posterior) {
    rv <- list(status="OK")
    rv$qtl <- posterior %>% 
                dplyr::summarise(QTL = mean({{metric}} >= range[1] & {{metric}} <= range[2])) %>% 
                dplyr::pull(QTL)
    for (n in names(probs)) {
      if (rv$qtl < probs[n]) {
        rv$status <- n
        break
      }
    }
    rv$data <- data %>% .addStatusToObservedData({{ statusCol }}, {{ observedMetric }}, lower, upper)
    rv
  }
  tryCatch({
    rv <- data %>% evaluateCustomQTL(posterior, func)
    },
    error=function(e) {
      rv <- list(
        "data"=NA,
        "status"="ERROR",
        "message"=e$message,
        "qtl"=NA
      )
      logger::log_error(e$message)
      rv
    }  
  )
  logger::log_debug("Exit")
  rv
}

#' Evaluates a QTL based on the proportion of site level KRIs within a range
#' 
#' Calculates the proportion of site-level KRIs that lie within a range
#' defined by quantiles of the posterior density provided by a call to a 
#' fit Bayes XXXX Model function.  
#' 
#' For more details, see Usage Notes below.
#' @param data the tibble containing site-level observed metrics (KRIs)
#' @param posterior the tibble containing the posterior distribution of the, 
#' metric, usually obtained from a fit Bayes model function.
#' @param metric the column in `posterior` on which the QTL should be based
#' @param observedMetric the column in `data` containing the site level 
#' observed metrics (KRIs)
#' @param statusCol the name of the column to be added to `data` that will
#' contain the site-level flags comparing the corresponding KRI to the QTL
#' thresholds defined by `lower` and `upper`
#' @param range the range of values of `metric` to be used when calculating
#' `qtl` = Prob(`metric` in `range`).
#' @param probs the thresholds for Prob(`metric` in `range`) that define the 
#' action and warning limits for the QTL
#' @param lower a scalar or vector of lower limits, or NULL.  See Usage Notes 
#' below.
#' @param upper a scalar or vector of upper limits, or NULL.  See Usage Notes 
#' below.
#' @param statusFunc a function that takes as its single argument the `qtl`
#' element of the return value and converts it to a status string
#' @return a list of three elements named `status`, `data` and `qtl`.  If
#' `status` is `"OK"`, no breach of the QTL was detected.  Otherwise, `status` 
#' is equal to name of the most serious breach of the QTL as determined by the 
#' values in either `lower` or `upper`. `qtl` is a tibble whose rows provide
#' counts of the number of sites whose KRIs lie in the ranges defined by `lower` 
#' and `upper`.  In addition, the `quantiles` element provides the conversion 
#' between quantiles of the posterior distribution of `metric` and values of
#' `metric` itself.
#' @section Usage Notes:
#' Either or both `lower` of `upper` can be NULL.  In what follows, `limit` refers to
#' both `lower` and `upper`.  
#' #' If `limit` has names, those names are used.  Otherwise, names are assigned 
#' based on the length of `limit`.  If `length(limit)` is `1`, the single 
#' element is named `"action"`.  If `length(limit)` is `2`, the two elements are
#' named `"warn"` and `"action"`.  (In this case, `limit` is first sorted
#' appropriately.)  If `length(limit)` is `>2`, its elements are named `"1"` to 
#' `"n"` where `n` is `length(limit)`.
#' 
#' If `statusFunc` is `NULL`, then the `status` element of the return value is
#' set to `"OK"` by default.
#' @examples 
#' berrySummary %>%
#' evaluateSiteMetricQTL(
#'   posterior=fitted$tab,
#'   metric=p,
#'   observedMetric=ObservedResponse,
#'   lower=c("action"=0.5, "warn"=0.6),
#'   upper=c("action"=0.9, "warn"=0.8),
#'   statusFunc=function(d) ifelse(
#'                            d %>% 
#'                              dplyr::filter(Status == "action") %>% 
#'                              dplyr::pull(N) > 1, "action", "OK"
#'                          )
#' )
#' @export
evaluateSiteMetricQTL <- function(
                           data,
                           posterior,
                           metric,
                           observedMetric,
                           statusCol=Status,
                           statusFunc=NULL,
                           lower=NULL,
                           upper=NULL
                         ) {
  logger::log_debug("Entry")
  logger::log_trace(deparse(match.call()))
  # Validate
  if (!is.data.frame(data)) stop("data is not a data.frame")
  data %>% .assertColumnExists({{ observedMetric }})
  data %>% .assertColumnDoesNotExist({{ statusCol }})
  posterior %>% .assertColumnExists({{ metric }})
  if (!is.null(statusFunc)) {
    if (!is.function(statusFunc)) stop("statusFunc is not  function")
  }
  if (is.null(lower) & is.null(upper)) stop("Both lower and upper cannot be NULL")
  if (!is.null(lower)) {
    if (any(is.na(lower))) stop("lower contains NAs")
    if (any(lower < 0) | any(lower > 1)) stop("lower contains values outside the range [0, 1]")
  }
  if (!is.null(upper)) {
    if (any(is.na(upper))) stop("upper contains NAs")
    if (any(upper < 0) | any(upper > 1)) stop("upper contains values outside the range [0, 1]")
  }
  # Prepare
  lower <- lower %>% .ensureLimitsAreNamed()
  upper <- upper %>% .ensureLimitsAreNamed(decreasing = TRUE)
  # Execute
  func <- function(data, posterior) {
    rv <- list(status="OK", quantiles=tibble::tibble())
    if (!is.null(lower)) {
      qLower <- quantile(posterior %>% dplyr::pull({{ metric }}), probs=lower)
      names(qLower) <- names(lower)
      x <- tibble:: tibble(Threshold="Lower", {{statusCol }} := names(lower), Quantile=lower, {{ metric }} := qLower)
      rv$quantiles <- rv$quantiles %>% dplyr::bind_rows(x)
    } else {
      qLower <- NULL
    }
    if (!is.null(upper)) {
      qUpper <- quantile(posterior %>% dplyr::pull({{ metric }}), probs=upper)
      names(qUpper) <- names(upper)
      x <- tibble:: tibble(Threshold="Upper", {{statusCol }} := names(upper), Quantile=upper, {{ metric }} := qUpper)
      rv$quantiles <- rv$quantiles %>% dplyr::bind_rows(x)
    } else {
      qUpper <- NULL
    }
    rv$quantiles <- rv$quantiles %>% dplyr::arrange(Quantile)
    rv$data <- data %>% .addStatusToObservedData({{ statusCol }}, {{ observedMetric }}, qLower, qUpper)
    rv$qtl <- rv$data %>%
                dplyr::group_by({{ statusCol }}) %>%
                dplyr::summarise(N=dplyr::n(), .groups="drop") %>% 
                dplyr::arrange({{ statusCol }})
    if (!is.null(statusFunc)) {
      rv$status <- rv$qtl %>% statusFunc()
    }
    rv
  }
  tryCatch({
    rv <- data %>% evaluateCustomQTL(posterior, func)
  },
  error=function(e) {
    rv <- list(
      "data"=NA,
      "status"="ERROR",
      "message"=e$message,
      "qtl"=NA
    )
    logger::log_error(e$message)
    rv
  }  
  )
  logger::log_debug("Exit")
  rv
}