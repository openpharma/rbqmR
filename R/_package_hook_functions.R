.onLoad <- function(libname, pkgname) {
  logger::log_layout(
    logger::layout_glue_generator(format = '{namespace} {time} {level} {fn}: {msg}'),
    namespace="rbqmR"
  )
}
