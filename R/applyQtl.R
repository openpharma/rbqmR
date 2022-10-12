#' @export
applyQtl <- function(data, var, lower=NULL, upper=NULL) {
  qLower <- rlang::enquo(lower)
  qUpper <- rlang::enquo(upper)
  
  d <- data
  if (!rlang::quo_is_null(qLower)) {
    d <- d %>% filter({{ var }} > !! qLower)
  }
  if (!rlang::quo_is_null(qUpper)) {
    d <- d %>% filter({{ var }} < !! qUpper)
  }
  
  return(data %>% anti_join(d) %>% add_column(Lower=lower, Upper=upper))
}