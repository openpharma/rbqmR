#' Summary Plot of Observed Event Rates/Proportions
#' 
#' @param data A tibble containing MCMC samples for which the density is 
#' required
#' @param siteData A tibble containing the observed site specific data
#' @param targetRange A tibble containing the lower and upper limits of the 
#' target range for the metric
#' @param actionLimits a list or named vector defining the action limits that
#' will appear on the graph. See `[shadeRange]` for details.  May be `NA` or 
#' `NULL`.
#' @param warningLimits a list or named vector defining the action limits that
#' will appear on the graph. See `[shadeRange]` for details.  May be `NA` or 
#' `NULL`.
#' @param observedMetric the observed value of the metric
#' @param siteSize The column in `siteData` that defines the sample size at each
#'  site.  Uses tidyselect. Ignored if `siteData` is `NULL`
#' @param siteMetric The column in `siteData` that defines the KRI at the site.
#'    Uses tidyselect. Ignored if `siteData` is `NULL`.
#' @export
createQtlPlot <- function(
                   data,
                   siteData=NULL,
                   targetRange=NULL,
                   actionLimits=NULL,
                   warningLimits=NULL,
                   observedMetric=NULL,
                   siteSize=NULL,
                   siteMetric=NULL
                 ) {
  plot <- data %>% 
            ggplot2::ggplot() +
              ggplot2::geom_density(ggplot2::aes(p), colour="grey") + 
              ggplot2::theme_light() +
              ggplot2::theme(
                axis.ticks.y =  ggplot2::element_blank(),
                axis.title.y =  ggplot2::element_blank(),
                axis.text.y =  ggplot2::element_blank()
              ) +
    ggplot2::labs(
      x="Event rate"
    )
  if (!is.null(actionLimits)) {
    plot <- plot %>% 
      shadeRange(
        range=actionLimits, 
        idx=1
      )
  }
  if (!is.null(warningLimits)) {
    plot <- plot %>% 
      shadeRange(
        range=actionLimits, 
        idx=1
      )
  }
  if (!is.null(targetRange)) {
    plot <- plot %>%  
              shadeRange(
                range=c(
                  targetRange, 
                  "alpha"=0.3, 
                  "colour"="steelblue2"
                ), 
                idx=1
              )
    plot <- plot +
              ggplot2::geom_vline(
                ggplot2::aes(xintercept=observedMetric)
              )
  }
  if (!is.null(siteData)) {
    divisor <- 5 * 
               ceiling(
                 max(siteData %>% dplyr::pull( {{siteSize}} ), rm.na=TRUE) /
                 max(ggplot2::ggplot_build(plot)$data[[1]]$density, rm.na=TRUE)/5
               )
    plot <- plot +
              ggplot2::geom_linerange(
                data=siteData,
                ggplot2::aes(
                  x={{ siteMetric }}, 
                  ymin=0, 
                  ymax={{ siteSize }}/divisor
                ),
                colour="darkslategrey"
              )
  }
  return(plot)
}
