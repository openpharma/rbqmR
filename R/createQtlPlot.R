#' Summary Plot of Observed Event Rates/Proportions
#'

#' @export
createQtlPlot <- function(
                   data,
                   qtl=NULL,
                   siteData=NULL,
                   targetRange=NULL,
                   observedMetric=NULL
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
  if (!is.null(qtl)) {
    plot <- plot %>% .shadeActionAndWarningRanges(limits=c(qtl$Q05, qtl$Q95), idx=1)
  }
  if (!is.null(targetRange)) {
    plot <- plot %>%  
              .shadeActionAndWarningRanges(
                limits=targetRange,
                rangeColours=c("white", "steelblue2", "white"),
                idx=1
              )
    print(observedMetric)
    plot <- plot +
              ggplot2::geom_vline(
                ggplot2::aes(xintercept=observedMetric)
              )
  }
  if (!is.null(siteData)) {
    # plot <- plot + 
    #           ggplot2::geom_linerange(
    #             data=siteData,
    #             ggplot2::aes(x=ObservedResponse, ymin=0, ymax=Subjects/20),
    #             colour="darkslategrey"
    #           )
  }
  return(plot)
}
