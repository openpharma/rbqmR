#https://en.wikipedia.org/wiki/Binomial_distribution#Ratio_of_two_binomial_distributions,
#Referencing Katz D. et al.(1978) Obtaining confidence intervals for the risk ratio
#in cohort studies. Biometrics 34:469–474

#' Create an Observed Over Expected Grid
#'  
#' Using the work of Katz et al (1978) calculate acceptable limits for the
#' ratio of two binomial proportions.  The Type 1 error rate can be specified,
#' and the limits can be one- or two-sided.
#' @param observedData a tibble containing the observed data that will be
#' compared to the calculated limit(s).  May be `NULL`.
#' @param observedRate  The column in `observedData` that contains the observed
#' rates.  Ignored if `observedData` is NULL.  Uses tidy evaluation.
#' @param n The column in both `observedData` (if not `NULL`) and the output
#' tibble that defines the sample size to which the corresponding limit(s)
#' relate(s).
#' @param nHistorical the number of observations on which the historical rate
#' is based
#' @param nObservedRange a sequence of values for which limits should be 
#' calculated
#' @param expectedRate the event rate expected in the current study.  Usually
#' the same as `historicalRate`, but need not be so.
#' @param historicalRate the event rate observed in the historical data
#' @param alpha The Type 1 error rate associated with the calculated limits.
#' Default 0.05
#' @param sides the sidedness of `alpha`.  Either `two`, `upper` or `lower`.
#' Default `two`.
#' @return a tibble
#' @export
createObservedOverExpectedTable <- function(
                                     observedData=NULL,
                                     observedRate=NULL,
                                     n=NObserved,
                                     nHistorical,
                                     nObservedRange=seq(50, 1500, 25),
                                     expectedRate,
                                     historicalRate,
                                     alpha=0.05,
                                     sides=c("two", "lower", "upper")
                                   ) {
  logger::log_debug("Entry")
  # Validate
  if (!is.null(observedData)) {
    if (!is.data.frame(observedData)) stop("observedData is not a data.frame")
    if (!(observedData %>% .columnExists({{ observedRate }}))) {
      stop(
        paste0(
          rlang::as_label(rlang::enquo(observedRate)),
          " is not a column in ",
          rlang::as_label(rlang::enquo(observedData))
        )
      )
    }
  }
  # Execute
  sides <- match.arg(sides)
  useAlpha <- ifelse(sides == "two", alpha / 2, alpha)

  data <- tibble::tibble(
            {{n}} := nObservedRange,
            NHistorical=nHistorical,
            ExpectedRate=expectedRate,
            HistoricalRate=historicalRate,
            Alpha=alpha,
            Sides=sides
          ) %>% 
          dplyr::mutate(
            Ratio=ifelse(HistoricalRate == 0, NA, ExpectedRate/HistoricalRate),
            LogRatio=ifelse(is.na(Ratio), NA, log(Ratio)),
            LogSD=sqrt(((1/ExpectedRate) - 1)/{{n}} + ((1/HistoricalRate) - 1)/NHistorical),
            LowerLog=LogRatio + stats::qnorm(useAlpha)*LogSD,
            UpperLog=LogRatio + stats::qnorm(1-useAlpha)*LogSD,
            Lower=exp(LowerLog),
            Upper=exp(UpperLog)
          ) %>% 
          dplyr::select(-tidyselect::contains("Log"))
  
  if (!is.null(observedData)) {
    data <- data %>%
              dplyr::left_join(observedData, by=rlang::quo_name(rlang::enquo(n)))
  }
  if (sides == "lower") {
    data <- data %>% dplyr::select(-Upper)
    # if (!is.null(observedData)) {
    #   data <- data %>%
    #     dplyr::mutate(
    #       Status=dplyr::case_when(
    #         # is.na({{ n }}) ~ NA,
    #         {{ observedRate }} < Lower ~ "BREACH",
    #         TRUE ~ "OK"
    #       )
    #     )
    # }
  }
  if (sides == "upper") {
    data <- data %>% dplyr::select(-Lower)
    # data <- data %>%
    #   dplyr::mutate(
    #     Status=dplyr::case_when(
    #       # is.na({{ n }}) ~ NA,
    #       {{ observedRate }} > Upper ~ "BREECH",
    #       TRUE ~ "OK"
    #     )
    #   )
  }
  if ( sides == "two") {
    # data <- data %>%
    #   dplyr::mutate(
    #     Status=dplyr::case_when(
    #       # is.na({{ n }}) ~ NA,
    #       {{ observedRate }} < Lower || {{ observedRate }} > Upper ~ "BREECH",
    #       TRUE ~ "OK"
    #     )
    #   )
  }
  logger::log_debug("Exit")
  return(data)
}
