#' Create an Observed Over Expected Plot
#' 
#' Given a tibble created by [`createObservedOverExpectedTable`] create an 
#' observed over expected plot.
#' 
#' @param data The tibble containing the calculated limits
#' @param n The column in `data` that defines the sample size to which the 
#' corresponding limit(s) relate(s)
#' @param observedRate  The column in `data` that defines the observed rate.  
#' May be `NULL`.
#' @param alpha The Type 1 error rate associated with the calculated limits.
#'  Default 0.05
#' @param sides the sidedness of `alpha`.  Either `two`, `upper` or `lower`.
#'  Default `two`.
#' @return a `ggplot2` object containing the requested plot
#' @export
createObservedOverExpectedPlot <- function(
                                    data,
                                    n=NObserved,
                                    observedRate=NULL,
                                    alpha=0.05,
                                    sides=c("two", "lower", "upper")
                                  ) {
  logger::log_debug("Entry")
  # Validate
  if (!is.data.frame(data)) stop("data is not a data.frame")
  if (!(data %>% .columnExists({{ n }}))) {
    stop(
      paste0(
        rlang::as_label(rlang::enquo(n)),
        " is not a column in ",
        rlang::as_label(rlang::enquo(data))
      )
    )
  }
  if (alpha <= 0 | alpha >= 1) stop("alpha must be strictly greater than zero and strictly less than one")
  # Execute
  sides <- match.arg(sides)
  
  plot <- data %>%
            ggplot2::ggplot() +
            ggplot2::geom_ribbon(ggplot2::aes(x={{ n }}, ymax=Upper, ymin=Lower), alpha=0.1, fill="steelblue1") +
            ggplot2::geom_hline(ggplot2::aes(yintercept=ExpectedRate/HistoricalRate), linetype="dotted") +
            ggplot2::theme_light() +
            ggplot2::labs(
              y="Ratio Observed/Expected",
              x="Evaluable subjects"
            )
  qObsRate <- rlang::enquo(observedRate)
  if (!rlang::quo_is_null(qObsRate)) {
    plot <- plot +
              ggplot2::geom_line(
                data=data %>% dplyr::filter(!is.na(!! qObsRate)),
                ggplot2::aes(
                  x= {{ n }} ,
                  y= {{ observedRate }}
                )
              )
  }
  logger::log_debug("Exit")
  return(plot)
}
