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
#' is equal to name of the most serous breach of the QTL as determined by the 
#' values in either `lower` or `upper`.
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
  logger::log_trace(match.call())
  # Validate
  if (is.null(lower) & is.null(upper)) stop("Both lowerThresholds and upperThresholds cannot be NULL")
  if (!(data %>% .columnExists({{ observedMetric }}))) {
    stop(
      paste0(
        rlang::as_label(rlang::enquo(observedMetric)),
        " is not a column in ",
        rlang::as_label(rlang::enquo(data))
      )
    )
  }
  if (!(posterior %>% .columnExists({{ metric }}))) {
    stop(
      paste0(
        rlang::as_label(rlang::enquo(metric)),
        " is not a column in ",
        rlang::as_label(rlang::enquo(posterior))
      )
    )
  }
  if (is.null(stat)) stop("stat cannot be NULL")
  if (!is.function(stat)) stop("stat is not a function")
  # Prepare
  if (!is.null(upper)) {
    # upper <- as.list(upper)
    if (is.null(names(upper))) {
      if (length(upper) == 1) names(upper) <- "action"
      else if (length(upper) == 2) {
        upper <- sort(upper, decreasing = TRUE)
        names(upper) <- c("action", "warn")
      }
      else names(upper) <- as.character(1:length(upper))
    }
  }
  if (!is.null(lower)) {
    # lower <- as.list(lower)
    if (is.null(names(lower))) {
      if (length(lower) == 1) names(lower) <- "action"
      else if (length(lower) == 2) {
        lower <- sort(lower)
        names(lower) <- c("action", "warn")
      }
      else names(lower) <- as.character(1:length(lower))
    } 
  }
  func <- function(data, posterior) {
    rv <- list("status"="OK")
    tryCatch({
      rv$data <- data %>% tibble::add_column({{ statusCol }} := "OK")
      rv$qtl <- posterior %>% 
                  dplyr::summarise(qtl=stat({{ metric }})) %>% 
                  dplyr::pull(qtl)
      for (x in names(lower)) {
        if (rv$qtl < lower[x]) rv$status <- x
        rv$data <- rv$data %>% 
                     dplyr::mutate(
                       {{statusCol}} := ifelse(
                                          {{ observedMetric }} < lower[x], 
                                          x, 
                                          {{ statusCol }}
                                        )
                     )
      }
      for (x in names(upper)) {
        if (rv$qtl > upper[x]) rv$status <- x
        rv$data <- rv$data %>% 
                     dplyr::mutate(
                       {{statusCol}} := ifelse(
                                          {{ observedMetric }} > upper[x], 
                                          x, 
                                          {{ statusCol }}
                                        )
                     )
      }
    },
    error=function(x) {
      rv$statsus="ERROR"
      rv$qtl=NA
    }
    )
    rv
  }
  rv <- data %>% evaluateCustomQTL(posterior, func)
  logger::log_debug("Exit")
  rv
}

