#' Fit an MCMC model to a dataset, capture and log JAGS messages
#'
#' @param data the JAGS data list
#' @param modelString The JAGS model string
#' @param monitor a vector of names of parameters to monitor
#' @param inits the inits used by the JAGS model
#' @param max.time the maximum time allowed for the model fitting,
#' @param returnClass the class to prepend to the class of the tab element of
#' of the returned value
#' @param ... other parameters passed to autorun.jags
#' @importFrom dplyr %>%
.autorunJagsAndCaptureOutput <- function(
    data,
    modelString,
    monitor,
    inits=NULL,
    returnClass="binomialModel",
    max.time=600, # 10 minutes,
    ...
) {
  futile.logger::flog.debug("Entry")
  futile.logger::flog.trace(deparse(match.call()))
  # Validate
  if (!is.list(data)) stop("data is not a data.frame")
  if (is.null(inits)) stop("inits cannot be NULL")
  # Execute
  ## always force summary, others set in zzz.R could be changed by user
  runjags::runjags.options(force.summary = TRUE)
  runjagsMessages <- utils::capture.output({
    # use autorun jags to ensure convergence diagnostics are met
    results <- try(
      runjags::autorun.jags(
        modelString,
        monitor = monitor,
        inits = inits,
        data = data,
        n.chains = length(inits),
        max.time = max.time,
        ...
      )
    )
  })
  # Log runjags messages
  invisible(lapply(runjagsMessages, function (y) futile.logger::flog.debug(y)))
  rv <- list()
  if (getOption("qtlanalysis.fitReturnFormat", "CURRENT") == "CURRENT") {
    if (inherits(results, "try-error")) {
      rv$status <- "ERROR"
      rv$results <- NULL
      futile.logger::flog.error(paste0(returnClass, " fit has returned a try-error object"), call. = TRUE)
    } else {
      tab <- dplyr::bind_rows(
               tibble::as_tibble(results$mcmc[[1]]),
               tibble::as_tibble(results$mcmc[[2]])
             )
      monitorNames <- stringr::str_replace(monitor, "\\[\\d+\\]", "")
      names(tab) <- monitorNames
      tab <- tab %>% dplyr::mutate(q = dplyr::ntile(!! {as.symbol(monitorNames[1])} , 100))
      class(tab) <- c(returnClass, class(tab))
      rv$tab <- tab
      rv$results <- results
      if (results$psrf$mpsrf > results$psrf$psrf.target) {
        rv$status <- "WARN"
        futile.logger::flog.warn(paste0("Target PSRF: ", results$psrf$psrf.target))
        futile.logger::flog.warn(paste0("Actual PSRF: ", results$psrf$mpsrf))
        futile.logger::flog.warn("Actual PSRF is potentially unacceptable.")
      } else {
        rv$status <- "OK"
      }
      futile.logger::flog.info(paste0("Status of model fitting: ", rv$status))
    }
    futile.logger::flog.debug("Exit")
    return(rv)
  } else {
    futile.logger::flog.debug("Exit")
    return(results)
  }
}
