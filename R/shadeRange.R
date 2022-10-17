#' Shade Areas Under The Curve
#'
#' Given an existing `ggplot2` object, shade an area under an existing density
#' curve.
#' @param plot The `ggplot2` object
#' @param range a `vector` (or `list of vectors`) containing the definition(s) of
#' the range(s) to be shaded.  See Usage Notes below.
#' @param idx An integer defining the index within the list returned by 
#' `ggplot2::ggpot_build` that identifies the layer containing the density to be
#' shaded.  Typicall, this corresponds to the order in which the various `geom`s
#' are added to the `ggplot2` object.  For example, in `d %>% ggplot2() %>% 
#' geom_point(...) %>% geom_density(...)`, `idx should be `2`.
#' @section Usage Notes:
#' `range` should be a vector or a list of vectors.  If a vector, it should contain 
#' named elements `lower`, `upper`, `alpha` and `colour`, where `lower` and
#' `upper` contain the lower and upper x axis values that define the extent of 
#' the shaded area, `colour` defines the colour of the shaded area and `alpha`
#' defines the alpha value that defines the transparency of the shading.  An
#' `alpha` of `0` denotes complete transparency (or invisibility).  An `alpha`
#' of 1 denotes total opacity, meaning that the contents of any layers below
#' the density will be invisible if within the shaded area.
#' 
#' To define shaded ranges that include the left- or right-most extents of the
#' density, set `lower` or `upper` respectively to `NA`.
#' 
#' If a list of vectors, each element of the  list should be a vector with 
#' elements as defined above.
#' 
#' For example, the default value of `range` defines the ranges to be shaded as
#' those that lie below the 10th centile or above the 90th centile of the
#' density in goldenrod1 using an alpha of 0.3.
#' @return The modified `ggplot2` object
#' @export 
shadeRange <- function(
                plot,
                range=list(
                  c("lower"=NA, "upper"=0.1, "alpha"=0.3, "colour"="goldenrod1"),
                  c("lower"=0.9, "upper"=NA, "alpha"=0.3, "colour"="goldenrod1")
                ),
                idx=2
              )
{
  #Validate
  if (is.null(plot)) stop("plot cannot be null")
  if (is.null(range)) stop("ranges cannot be null")
  if (!is.list(range) & !is.vector(range)) stop("range must be a list or vector")

  d <- ggplot2::ggplot_build(plot)$data[[idx]] #2 (or 3) because the MCMC density is the second (or third) component of the ggplot created by createXXXXQtlPlot()
  d$Facet <- "Density" # so only adds to density facet

  
  if (is.list(range)) {
    for (r in range) {
      plot <- shadeRange(plot, r, idx)
    }
  } else {
    if (!is.null(range) & !is.na(range)) {
      if (!is.vector(range)) stop("ranges must be a list or named vector")
      if (length(setdiff(names(range), c("upper", "lower", "colour", "alpha"))) != 0)
        stop("range is not named correctly")
    }
    if (!(is.null(range) & is.na(range()))) {
      if (is.na(range["lower"])) range["lower"] <- -Inf
      if (is.na(range["upper"])) range["upper"] <- Inf
      inRange <- with(d, x >= range["lower"] & x <= range["upper"])
      ok <- any(inRange)
      if (ok) {
        plot <- plot +
                  ggplot2::geom_area(
                    data = d[inRange,],
                    ggplot2::aes(x = x, y = y), 
                    alpha = range["alpha"],
                    fill = range["colour"], 
                    inherit.aes = FALSE
                  )
      } else {
        logger::log_warn(
           paste0(
             "The MCMC density has no fitted points between ", range["lower"],
                " and ", range["upper"],
                ". One solution may be to increase nDensity, ensuring it remains a power of two"
           )
        )
      }
    }
  }
  return (plot)
}