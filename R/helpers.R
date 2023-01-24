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

#' Throw an exception of the given column DOES NOT exist in the given data.frame
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

#' Throw an exception of the given column DOES exist in the given data.frame
#' 
#' @param data the data.frame to be searched
#' @param col the name of the column to be searched
#' @return `NULL`
#' @examples 
#' # tibble::tibble(good=1) %>% .assertColumnExists(good) # No exception
#' # tibble::tibble(good=1) %>% .assertColumnExists(bad) # Exception
.assertColumnDoesNotExist <- function(data, col) {
  if (data %>% .columnExists({{ col }})) {
    stop(
      paste0(
        rlang::as_label(rlang::enquo(col)),
        " already exists in ",
        rlang::as_label(rlang::enquo(data))
      )
    )
  }
}

#' Ensures that a vector or scalar is named according to standard rules
#' 
#' @param x the vector (or scalar) to be named
#' @param decreasing if naming is required, how should `x` be sorted?
#' @param nameStrings an optional vector that provides the names to be used.
#' See Usage Notes below
#' @section Usage Notes:
#' If `nameStrings` is not `NULL`, its length must be greater than or equal to
#' the length of `x`.  Otherwise, names are determined as follows:
#' If `length(x)` is 
#' `1`, the name of `nameString`'s single element is `"action"`
#' `2`, the names of `nameString`'s elements are `"action"` and `"warn"`, in 
#' that order
#' `2` or more, `nameString`'s elements are the character equivalent of the
#'  integers `1` to `length(x)`.
#' Otherwise t
#' @return If `x` is `NULL` or named, it is returned unaltered.  Otherwise,
#' it is first sorted according to `descending` and then names from `names` are
#' assigned in order.
.ensureLimitsAreNamed <- function(x, decreasing=FALSE, nameStrings=NULL) {
  futile.logger::flog.debug("Entry")
  futile.logger::flog.trace(deparse(match.call()))
  # Validate
  if (!is.null(x)) {
    if (!is.logical(decreasing)) stop("decreasing must be a logical")
    if (!is.null(nameStrings)) {
      if (length(x) > length(nameStrings)) stop("nameStrings is too short to provide names for every element of limits")
    }
  }
  # Execute
  if (is.null(x)) {
    futile.logger::flog.debug("Exit [NULL]")
    return(NULL)
  }
  if (!is.null(names(x))) {
    futile.logger::flog.debug("Exit [named]")
    return(x)
  }
  x <- sort(x, decreasing = decreasing)
  if (is.null(nameStrings)) {
    if (length(x) == 1) {
      nameStrings <- c("action")
    } else if (length(x) == 2) {
      nameStrings <- c("action", "warn")
    } else {
      nameStrings <- as.character(1:length(x))
    }
  }
  names(x) <- nameStrings[1: length(x)]
  futile.logger::flog.debug("Exit")
  x
}

.addStatusToObservedData <- function(data, statusCol, observedMetric, lower, upper) {
  futile.logger::flog.debug("Entry")
  futile.logger::flog.trace(deparse(match.call()))
  # Validate
  if (!is.data.frame(data)) stop("data is not a data.frame")
  data %>% .assertColumnExists({{ observedMetric }})
  # Execute
    rv <- data %>% tibble::add_column({{ statusCol }} := "OK")
    # Need to ensure limits are tested from least to most severe
    if (length(lower) > 0) {
      for (i in length(lower):1) {
        x <- names(lower)[i]
        rv <- rv %>% 
          dplyr::mutate(
            {{statusCol}} := ifelse(
              {{ observedMetric }} < lower[x], 
              x, 
              {{ statusCol }}
            )
          )
      }
    }
    if (length(upper) > 0) {
      for (x in names(upper)) {
        rv <- rv %>% 
          dplyr::mutate(
            {{statusCol}} := ifelse(
              {{ observedMetric }} > upper[x], 
              x, 
              {{ statusCol }}
            )
          )
      }
  }
  futile.logger::flog.debug("Exit")
  return(rv)
}