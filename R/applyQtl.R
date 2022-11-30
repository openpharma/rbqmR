#' Filter a Tibble to Obtain Values Outside a QTL
#' 
#' Given an input tibble and a column, filter the tibble to retain only those
#' rows in which the value of the column lies outside the range given by 
#' upper and lower limits, both of which are optional.
#' @param data the input tibble
#' @param var the name of the column on which to filter.  Uses NSE.
#' @param lower The lower limit.  May be `NULL`.
#' @param upper The upper limit.  May be `NULL`.
#' @return the filtered tibble
#' @export
applyQtl <- function(
              data, 
              var, 
              lower=NULL, 
              upper=NULL
            ) {
  # Validate
  if (!is.data.frame(data)) stop("data is not a data.frame")
  if (!(data %>% .columnExists({{ var }}))) {
    stop(
      paste0(
        rlang::as_label(rlang::enquo(var)),
        " is not a column in ",
        rlang::as_label(rlang::enquo(data))
      )
    )
  }
  if (is.null(lower) & is.null(upper)) stop("Both lower and upper cannot be NULL")
  # Execute
  qLower <- rlang::enquo(lower)
  qUpper <- rlang::enquo(upper)
  
  d <- data
  if (!rlang::quo_is_null(qLower)) {
    d <- d %>% dplyr::filter({{ var }} > !! qLower)
  }
  if (!rlang::quo_is_null(qUpper)) {
    d <- d %>% dplyr::filter({{ var }} < !! qUpper)
  }
  
  return(
    data %>% 
      dplyr::anti_join(d, by=rlang::as_label(rlang::enquo(var))) %>% 
      tibble::add_column(Lower=lower, Upper=upper))
}