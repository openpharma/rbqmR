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
  futile.logger::flog.debug("Entry")
  futile.logger::flog.trace(deparse(match.call()))
  # Validate
  if (!is.null(observedData)) {
    if (!is.data.frame(observedData)) stop("observedData is not a data.frame")
    observedData %>% .assertColumnExists({{ observedRate }})
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
  }
  if (sides == "upper") {
    data <- data %>% dplyr::select(-Lower)
  }
  futile.logger::flog.debug("Exit")
  return(data)
}
