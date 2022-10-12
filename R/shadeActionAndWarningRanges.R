.shadeActionAndWarningRanges <- function(plot,
                                        limits = c(0.1, 0.2, 0.8, 0.9),
                                        rangeAlpha=0.3,
                                        # rangeColours=c(c("#011506", "#0B3F05",
                                        #                  "#187008", "#25A30E")),
                                        rangeColours=c(c("darkgoldenrod1", "white",
                                                         "darkgoldenrod2")),
                                        idx=2)
{
  #Validate
  if (is.null(plot)) stop("Plot cannot be null")

  d <- ggplot_build(plot)$data[[idx]] #2 (or 3) because the MCMC density is the second (or third) component of the ggplot created by createXXXXQtlPlot()
  d$Facet <- "Density" # so only adds to density facet

  # extend limits and colours
  limits <- c(-Inf, limits, Inf)
  n <- length(limits)
  rangeColours <- c(rangeColours, NA, rev(rangeColours))
  for (i in seq(n - 1)){
    if (is.na(rangeColours[i])) next
    inRange <- with(d, x >= limits[i] & x <= limits[i + 1])
    ok <- any(inRange)
    if (ok) {
      plot <- plot +
        geom_area(data = d[inRange,],
                  aes_string(x = "x", y = "y"), alpha = rangeAlpha,
                  fill = rangeColours[i], inherit.aes = FALSE)
    } else {
      warning("The MCMC density has no fitted points between ", limits[i],
              " and ", limits[i + 1],
              ". Increase nDensity, ensuring it remains a power of two")
    }
  }
  return (plot)
}