#' Create an Observed Minus Expected Plot
#' 
#' Given an input `data.frame`, create the corresponding observed-expected plot with
#' option upper and lower warning limits and optional upper and lower action 
#' limits
#' 
#' @param data The input `data.frame`
#' @param indexVar The column in `data` that defines the sequence in which 
#' participants enrolled.  Should be an intger vector with values from `1` to `n`,
#' where `n` is the number of participants enrolled.
#' @param upperActionLimit The column in `data` that defines the upper action limit.
#' If `NULL`, the upper action limit is omitted from the plot.
#' @param lowerActionLimit The column in `data` that defines the lower action limit.
#' If `NULL`, the lower action limit is omitted from the plot.
#' @param upperWarningLimit The column in `data` that defines the upper warning limit.
#' If `NULL`, the upper warning limit is omitted from the plot.
#' @param lowerWarningLimit The column in `data` that defines the lower warning limit.
#' If `NULL`, the lower warning limit is omitted from the plot.
#' @param warningQuantiles A named numeric vector with elements `upper` and `lower`
#' containing the quantiles of the binomial distribution that define the warning limits.  
#' Used only for labeling.  If `NULL`, the corresponding footer is omitted
#' from the plot.  Individual elements may be `NA`.
#' @param permittedRates A named numeric vector with elements `upper` and `lower`
#' containing the observed event rates that define the action limits.  
#' Used only for labeling.  If `NULL`, the corresponding footer is omitted
#' from the plot.  Individual elements may be `NA`.
#' @export
createObservedMinusExpectedPlot <- function(
    data,
    indexVar=SubjectIndex,
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
  if (!(data %>% .columnExists({{ indexVar }}))) {
    stop(
      paste0(
        rlang::as_label(rlang::enquo(indexVar)),
        " is not a column in ",
        rlang::as_label(rlang::enquo(data))
      )
    )
  }
  # Execute
  plot <- data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(y=ObservedMinusExpected, x={{ indexVar }}), colour="grey") +
    ggplot2::geom_hline(ggplot2::aes(yintercept=0), colour="green") +
    ggplot2::labs(
      x="Cumulative Number of Subjects",
      y="Observed-Expected"
    ) +
    ggplot2::theme_light() 
  caption <- ""
  if(!is.null(warningQuantiles)) {
    caption <- paste0(
      "Warning quantiles - lower: ",
      toOrdinal::toOrdinal(
        100*warningQuantiles["lower"],
        getOption("rbqmR.language.toOrdinal", "English")
      ),
      ", upper: ",
      toOrdinal::toOrdinal(
        100*warningQuantiles["upper"],
        getOption("rbqmR.language.toOrdinal", "English")
      )
    )
  }
  if (!is.null(permittedRates)) {
    if (stringr::str_length(caption) > 0) {
      caption <- paste0(caption, "\n")
    }
    caption <- paste0(caption, "Action limits - lower: ", 100*permittedRates["lower"], "%, upper: ", 100*permittedRates["upper"], "%")
  }

  if(stringr::str_length(caption) > 0) {
    plot <- plot + ggplot2::labs(caption=caption)
  }

  qUpperActionLimit <- rlang::enquo(upperActionLimit)
  if (!rlang::quo_is_null(qUpperActionLimit)) {
    plot <- plot + ggplot2::geom_hline(ggplot2::aes(yintercept=!! qUpperActionLimit), colour="red")
  }
  if (!is.null(permittedRates) && !is.na(permittedRates["lower"])) {
    plot <- plot + ggplot2::geom_hline(ggplot2::aes(yintercept={{ lowerActionLimit }}), colour="red")
  }
  qLowerWarningLimit <- rlang::enquo(lowerWarningLimit)
  if (!rlang::quo_is_null(qLowerWarningLimit)) {
    plot <-  plot  +
      ggplot2::geom_line(ggplot2::aes(y=!! qLowerWarningLimit, x={{ indexVar }}), colour="gold")
  }
  qUpperWarningLimit <- rlang::enquo(upperWarningLimit)
  if (!rlang::quo_is_null(qUpperWarningLimit)) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(y=!! qUpperWarningLimit, x={{ indexVar }}), colour="gold")
  }
  logger::log_debug("Exit")
  return(plot)
}
