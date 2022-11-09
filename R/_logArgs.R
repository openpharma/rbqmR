#' @export
.logArgs <- function(args, indent=2, logLevel=logger::TRACE) {
  if (length(args) == 1) {
    val <- dplyr::case_when(
             is.symbol(x) ~  deparse(substitute(x)),
             TRUE ~ "Unknown"
           )
    logger::log_level(logLevel, paste0(stringr::str_dup(" ", indent), names(args), ": ", val))
  } else {
    lapply(
      names(args[2:length(args)]),
      function(x) .logArgs(x, indent+2)
    )    
  }

}


x <- getModelString("poisson")

x1 <- stringr::str_replace_all(x, "\\{", "\\{\\{")
stringr::str_replace_all(x1, "\\}", "\\}\\}")


stringr::str_replace_all(x, c("\\{"="\\{\\{", "\\}"="\\}\\}"))
