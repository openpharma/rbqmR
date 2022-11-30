#' Create a data.frame containing the results of an Observed - Expected Analysis
#' 
#' @param data The input `data.frame`.  Required.
#' @param timeVar The column in `data` that defines the order in which 
#' participants enrolled. Required.
#' @param eventVar The column in `data` that defines the whether or not a 
#' participant experienced an event. Required.
#' @param eventArray  An array defining the values of `eventVar` that indicate 
#' that the corresponding participant experienced an event.  Required.
#' @param expectedRate A scalar that defines the participant-level event rate
#' that is expected.  Required.
#' @param cumulativeVar The name of the column in the output `data.frame` that
#' contains the cumulative sum of events.
#' Uses tidy evaluation.
#' @param observedMinusExpectedVar The name of the column in the output `data.frame` that
#' contains the calculated observed - expected statistic for each participant.
#' Uses tidy evaluation.
#' @param indexVar The name of the column in the output `data.frame` that
#' contains the index of the participant's enrollment: an integer from `1` to `n`
#' where `n` is number of participants enrolled. Uses tidy evaluation.
#' @param statusVar The name of the column in the output `data.frame` that
#' contains the status of the trial: either `OK`, `WARN` or `BREECH`
#' @param maxTrialSize A scalar integer defining the maximum possible size of 
#' the trial.  Can be `NULL`.
#' @param upperActionLimit The name of the column in the output `data.frame` that
#' contains the upper action limit for this participant.  Uses tidy evaluation.
#' @param lowerActionLimit The name of the column in the output `data.frame` that
#' contains the lower action limit for this participant.  Uses tidy evaluation.
#' @param upperWarningLimit The name of the column in the output `data.frame` that
#' contains the upper warning limit for this participant.  Uses tidy evaluation.
#' @param lowerWarningLimit The name of the column in the output `data.frame` that
#' contains the lower warning limit for this participant.  Uses tidy evaluation.
#' @param warningQuantiles a named numeric vector of length 2 with elements 
#' `lower` and `upper` that define the quantiles of the binomial distribution
#' that define the warning limits.  Individual elements may be `NA`.  If `NULL`, 
#' `lowerWarningLimit` and `upperWarningLimit` are ignored and the corresponding 
#' columns are not added to the output `data.frame`
#' @param permittedRates a named numeric vector of length 2 with elements 
#' `lower` and `upper` that define the observed event rates that define the 
#' action limits.  Individual elements may be `NA`.  If `NULL`, 
#' `lowerActionLimit` and `upperActionLimit` are ignored and the corresponding 
#' columns are not added to the output `data.frame`
#' @importFrom rlang :=
#' @export
createObservedMinusExpectedTable <- function(
                           data,
                           timeVar,
                           eventVar,
                           eventArray,
                           expectedRate,
                           cumulativeVar=CumulativeEvents,
                           observedMinusExpectedVar=ObservedMinusExpected,
                           indexVar=SubjectIndex,
                           statusVar=Status,
                           maxTrialSize=NA,
                           upperActionLimit=UpperActionLimit,
                           lowerActionLimit=LowerActionLimit,
                           upperWarningLimit=UpperWarningLimit,
                           lowerWarningLimit=LowerWarningLimit,
                           warningQuantiles=c("lower"=0.01, "upper"=0.99),
                           permittedRates=c("lower"=0.05, "upper"=0.15)
                         ) {
  logger::log_debug("Entry")
  logger::log_trace(match.call())
  # Validate
  if (!is.data.frame(data)) stop("data is not a data.frame")
  if (!(data %>% .columnExists({{ timeVar }}))) {
    stop(
      paste0(
        rlang::as_label(rlang::enquo(timeVar)),
        " is not a column in ",
        rlang::as_label(rlang::enquo(data))
      )
    )
  }
  if (!(data %>% .columnExists({{ eventVar }}))) {
    stop(
      paste0(
        rlang::as_label(rlang::enquo(eventVar)),
        " is not a column in ",
        rlang::as_label(rlang::enquo(data))
      )
    )
  }
  # Execute
  if (!is.vector(eventArray)) eventArray <- as.vector(eventArray)
  rv <- data %>% 
          dplyr::arrange( {{ timeVar }}) %>% 
          dplyr::mutate(
            {{ indexVar }} := dplyr::row_number(),
            {{ cumulativeVar }} := cumsum({{ eventVar }} %in% eventArray),
            {{ observedMinusExpectedVar }} := {{ cumulativeVar }} - ({{ indexVar }} * expectedRate),
            {{ statusVar }} := "OK"
          )
  qLowerWarningLimit <- rlang::enquo(lowerWarningLimit)
  if (!rlang::quo_is_null(qLowerWarningLimit)) {
    rv <- rv %>%
      dplyr::mutate(
        LowerWarningLimit := stats::qbinom(warningQuantiles["lower"], {{indexVar}}, expectedRate) - (expectedRate*{{indexVar}}),
        {{ statusVar }} := ifelse({{ observedMinusExpectedVar }} < !! qLowerWarningLimit, "WARN", {{ statusVar }})
      )
  }
  qUpperWarningLimit <- rlang::enquo(upperWarningLimit)
  if (!rlang::quo_is_null(qUpperWarningLimit)) {
    rv <- rv %>%
      dplyr::mutate(
        !! qUpperWarningLimit := stats::qbinom(warningQuantiles["upper"], {{indexVar}}, expectedRate) - (expectedRate*{{indexVar}}),
        {{ statusVar }} := ifelse({{ observedMinusExpectedVar }} > !! qUpperWarningLimit, "WARN", {{ statusVar }}))
  }
  if (!is.na(maxTrialSize)) {
    qLowerActionLimit <- rlang::enquo(lowerActionLimit)
    if (!rlang::quo_is_null(qLowerActionLimit)) {
      rv <- rv %>%
        dplyr::mutate(
          !! qLowerActionLimit := maxTrialSize * (permittedRates["lower"] - expectedRate),
          {{ statusVar }} := ifelse({{ observedMinusExpectedVar }} < !! qLowerActionLimit, "BREECH", {{ statusVar }})
        )
    }
    qUpperActionLimit <- rlang::enquo(upperActionLimit)
    if (!rlang::quo_is_null(qUpperActionLimit)) {
      rv <- rv %>%
        dplyr::mutate(
          !! qUpperActionLimit := maxTrialSize * (permittedRates["upper"] - expectedRate),
          {{ statusVar }} := ifelse({{ observedMinusExpectedVar }} > !! qUpperActionLimit, "BREECH", {{ statusVar }})
          
        )
    }
  }
  logger::log_debug("Exit")
  return(rv)
}