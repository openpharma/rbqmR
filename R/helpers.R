#' Determine if a column, passed using NSE, exists in a data.frame
#' 
#' @param data the data.frame to be searched
#' @param col the name of the column to be searched
#' @return `TRUE` if the column exists, `FALSE` otherwise
#' @examples 
#' # mtcars %>% .columnExists(mpg)
#' # mtcars %>% .columnExists(bad)
.columnExists <- function(data, col) {
  # Validate
  if (is.null(data)) stop("data cannot be NULL")
  if (!is.data.frame(data)) stop("data is not a data.frame")
  # Execute
  rlang::as_label(rlang::enquo(col)) %in% names(data)
}