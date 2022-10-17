#' @export
applyQtl <- function(data, var, lower=NULL, upper=NULL) {
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
      dplyr::anti_join(d) %>% 
      tibble::add_column(Lower=lower, Upper=upper))
}