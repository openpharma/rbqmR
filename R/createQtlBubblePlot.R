#' Create a QTL bubble plot
#'
#' @param d the data frame containing the data to be plotted
#' @param x <[`data-masked`][dplyr::dplyr_data_masking]> the column in `data` that should appear on the x-axis
#' @param y <[`data-masked`][dplyr::dplyr_data_masking]> the column in `data` that should appear on the y-axis
#' @param size <[`data-masked`][dplyr::dplyr_data_masking]> the column in `data` that defines the size of the bubbles
#' @param group <[`data-masked`][dplyr::dplyr_data_masking]> Default: `NULL` the column in `data` that defines the fill colour of the bubbles
#' @param limits Default: `NULL` a `list` defining the characteristics of the horizontal reference lines that should appear on the plot.  See Usage notes below.
#' @param summarise Default: `FALSE` Should the input data frame be summarised to derive the plotted statistics?  See Usage notes below
#' @param summarize Default: `summarise`  A US English equivalent to `summarise`
#' @param stat Default: `NULL`  The summary function to be used to summarise the data.  Cannot be `NULL` if `summarise` is `TRUE`.  See Usage notes below
#' @param jitterHeight Default: `0`.  Value of the `height` parameter in the call to `geom_jitter`.  The default value implies no vertical jittering.
#' @param jitterWidth Default: `NULL`.  Value of the `width` parameter in the call to `geom_jitter`.  The default value implies a 40% horizontal jitter.
#' @param showBox Default: `TRUE` show an unjittered box-and-whister plot at each value of `x`?.  
#' @param boxFill Default: `"steelblue2"`: the colour with which the box-and-whisker plot is filled
#' @param boxColour Default: `boxFill`: the colour with which the outline of the box-and-whisker plot is drawn
#' @param boxColor Default: `boxColour`: US-English version of `boxColour`
#' @param boxAlpha Default: `0.25` The transparency of the box-and-whisker plot.  If `alpha=0`, the box is invisible.  If `alpha=1`, the box is totally opaque.
#' @param boxWidth Default: `0.5` The width of each box in the box-and-whisker plot, in the same units as those used on the x-axis
#' @param showLegend Default: `TRUE`  Should the legend defined by `group` appear in the plot
#' @param ... additional parameters passed to ggplot functions and `stat`
#' @return a ggplot object
#'
#' @section Usage notes:
#' `limits` should be a list of lists, each element of which defines the label (default `""`), `colour` (default `"black"`), line `type` (default `"solid"`), `y` co-ordinate (default `1`), `x` co-ordinate (default `0`), and vertical offset (`vjust`) (default `-1`) of a reference line.  If any element of the component lists is missing, a warning is issued and a default value supplied.  For example:  `limits=list(list(label="QTL (12%)", colour="red", type="dashed", y=12, x=1.5, vjust=-1),list(label="Sec Lim (8%)", colour="goldenrod", type="dotted", y=58, x=1.25, vjust=-1))`
#'
#' If `summarise` is `TRUE`, the (participant-level) information in `data` is summarised before the plot is created.  `data` is grouped by `group` and `x` and then `y=stat(y)` and `size=sum(size)` are calculated, where `x`, `y`, `size` and `stat` are the parameters defined above.
#' @export
createQtlBubblePlot <- function(d,
                                x,
                                y,
                                size,
                                group = NULL,
                                limits = NULL,
                                summarise = FALSE,
                                summarize = summarise,
                                stat = NULL,
                                jitterHeight=0,
                                jitterWidth=NULL,
                                showBox = TRUE,
                                boxFill = "steelblue2",
                                boxColour = boxFill,
                                boxColor = boxColour,
                                boxAlpha = 0.25,
                                boxWidth = 0.5,
                                showLegend = TRUE,
                                ...) {
  logger::log_debug("Entry")
  logger::log_trace(deparse(match.call()))
  # Validate
  if (is.null(d)) stop("data cannot be NULL")
  d %>% .assertColumnExists({{ x }})
  d %>% .assertColumnExists({{ y }})
  d %>% .assertColumnExists({{ size }})
  if (!rlang::quo_is_null(rlang::enquo(group))) {
    d %>% .assertColumnExists({{ group }})
  }
  if (!identical(summarise, summarize)) stop("summarise and summarize are inconsistent")
  if (isTRUE(summarise)) {
    if (!is.function(stat)) stop("stat must be a function when summarise is TRUE")
  }
  limitDefaults <- list("label" = "", "colour" = "black", "type" = "solid", "y" = 1, "x" = 0, "vjust" = -1)
  if (!is.null(limits)) {
    if (!is.list(limits)) stop("limits is not a list")
    for (i in seq_along(limits)) {
      lim <- limits[[i]]
      if (!is.list(lim)) stop(paste0("Element ", i, " of limits is not a list"))
      for (j in names(limitDefaults)) {
        if (!(j %in% names(lim))) {
          logger::log_warn(paste0("'", j, "' is not an element of limits[[", i, "]]. Adding default value ", limitDefaults[[j]], "..."))
          limits[[i]][[j]] <- limitDefaults[[j]]
        }
      }
    }
  }
  # Execute
  if (isTRUE(summarise)) {
    d <- d %>%
      dplyr::group_by({{ group }}, {{ x }}) %>%
      dplyr::summarise(
        {{ y }} := stat({{ y }}, ...),
        {{ size }} := sum({{ size }}),
        .groups = "drop"
      )
  }
  if (rlang::quo_is_null(rlang::enquo(group))) {
    p <- d %>% ggplot2::ggplot(ggplot2::aes(x = {{ x }}, y = {{ y }}, size = {{ size }}), ...)
  } else {
    p <- d %>%
      ggplot2::ggplot(ggplot2::aes(x = {{ x }}, y = {{ y }}, colour = {{ group }}, size = {{ size }}), ...)
  }
  p <- p + 
       ggplot2::guides(size = "none") +
       ggplot2::geom_jitter(height = jitterHeight, width = jitterWidth)
  if (showBox) {
    p <- p + 
           ggplot2::geom_boxplot(
             ggplot2::aes(group = {{ x }}), 
             fill = boxFill, 
             colour = boxColour, 
             alpha = boxAlpha, 
             show.legend = FALSE, 
             outlier.shape = NA,     # Hides outlier points, which are already plotted by geom_jitter
             width = boxWidth,
             ...
           )
  }
  for (i in seq_along(limits)) {
    lim <- limits[[i]]
    p <- p +
      ggplot2::geom_hline(
        yintercept = lim$y,
        colour = lim$colour,
        linetype = lim$type
      ) +
      ggplot2::annotate(
        "text",
        y = lim$y,
        x = lim$x,
        label = lim$label,
        colour = lim$colour,
        vjust = lim$vjust
      )
  }
  if (!showLegend) {
    p <- p + ggplot2::guides(fill = "none")
  }
  logger::log_debug("Exit")
  p
}
