#' Determine if a column, passed using NSE, exists in a data.frame
#' 
#' @param data the data.frame to be searched
#' @param col the name of the column to be searched
#' @return `TRUE` if the column exists, `FALSE` otherwise
#' @examples 
#' # tibble::tibble(good=1) %>% .columnExists(good) # TRUE
#' # tibble::tibble(good=1) %>% .columnExists(bad) # FALSE
.columnExists <- function(data, col) {
  # Validate
  if (is.null(data)) stop("data cannot be NULL")
  if (!is.data.frame(data)) stop("data is not a data.frame")
  # Execute
  rlang::as_label(rlang::enquo(col)) %in% names(data)
}

#' Throw an exception of the given column does not exist in the given data.frame
#' 
#' @param data the data.frame to be searched
#' @param col the name of the column to be searched
#' @return `NULL`
#' @examples 
#' # tibble::tibble(good=1) %>% .assertColumnExists(good) # No exception
#' # tibble::tibble(good=1) %>% .assertColumnExists(bad) # Exception
.assertColumnExists <- function(data, col) {
  if (!(data %>% .columnExists({{ col }}))) {
    stop(
      paste0(
        rlang::as_label(rlang::enquo(col)),
        " is not a column in ",
        rlang::as_label(rlang::enquo(data))
      )
    )
  }
}