#' Create a JAGS inits suitable for use with run.jags and autorun.jags
#'
#' @param quantiles Default `NULL`. If `NULL`, starting values of `a` and
#' `b` are chosen at random.  Otherwise, starting values are defined by the 
#' values supplied in `quantiles`. See Usage Notes below.
#' @param gammaShape a named vector defining the shape and scale parameters of
#' the gamma hyperprior for the shape parameter 
#' @param gammaScale a named vector defining the shape and scale parameters of
#' the gamma hyperprior for the scale parameter 
#' @param seed the seed to use when generating random seeds for the MCMC processes
#' @param rng the random number generator to be used for each init
#' @param n the length of the initialisation vector for `lambda`
#'@export
.createPoissonInit <- function(
    gammaShape=c(shape=1, scale=10),
    gammaScale=c(shape=1, scale=10),
    seed=NULL,
    quantiles=NULL,
    rng = c(
      "base::Mersenne-Twister", 
      "base::Wichmann-Hill", 
      "base::Marsaglia-Multicarry", 
      "base::Super-Duper"
    ),
    n=1
) {
  futile.logger::flog.debug("Entry")
  futile.logger::flog.trace(deparse(match.call()))
  
  rng <- match.arg(rng)

  # Validate
  if (!setequal(names(gammaShape), c("shape", "scale"))) stop("elements of gammaShape are not named 'shape' and 'scale'.")
  if (!setequal(names(gammaScale), c("shape", "scale"))) stop("elements of gammaScale are not named 'shape' and 'scale'.")
  if (!is.null(quantiles)) {
    if (!setequal(names(quantiles), c("shape", "scale"))) stop("elements of quantiles are not named 'shape' and 'scale'.")
    if (max(quantiles, na.rm=TRUE) >= 1) stop("not all elements of quantiles are less than 1.")
    if (min(quantiles, na.rm=TRUE) <= 0) stop("not all elements of quantiles are greater than 0.")
  }
  if (min(gammaShape, na.rm=TRUE) <= 0) stop("not all elements of gammaShape are greater than 0.")
  if (min(gammaScale, na.rm=TRUE) <= 0) stop("not all elements of gammaScale are greater than 0.")
  
  if (gammaShape["shape"] < 1 | gammaShape["scale"] < 1) {
    futile.logger::flog.warn("At least one element of gammaShape is less than 1.  It is recommended that both elements are greater than 1 so that the hyperprior is not U-shaped.")
    warning("At least one element of gammaA is less than 1.  It is recommended that both elements are greater than 1 so that the hyperprior is not U-shaped.")
  }
  if (gammaScale["shape"] < 1 | gammaScale["scale"] < 1) {
    futile.logger::flog.warn("At least one element of gammaScale is less than 1.  It is recommended that both elements are greater than 1 so that the hyperprior is not U-shaped.")
    warning("At least one element of gammaB is less than 1.  It is recommended that both elements are greater than 1 so that the hyperprior is not U-shaped.")
  }
  if (!is.null(seed)) {
    if (seed < 0 | seed != as.integer(seed)) stop("Seed must be a positive integer")
  }
  if (!is.null(quantiles)) {
    if (min(quantiles, na.rm=TRUE) <= 0) stop("not all elements of quantiles are greater than 0.")
    if (max(quantiles, na.rm=TRUE) >= 1) stop("not all elements of quantiles are less than 1.")
    if (!setequal(names(quantiles), c("shape", "scale"))) stop("elements of quantiles are not named 'shape' and 'scale'.")
  }
  
  # Execute
  init <- list(
    ".RNG.name"=rng, 
    ".RNG.seed"=ifelse(is.null(seed), floor(stats::runif(1, max=.Machine$integer.max)), seed),
    "lambda"=stats::rgamma(n, shape=1, scale=1)
  )
  if (is.null(quantiles)) {
    init$shape <- stats::rgamma(1, shape=gammaShape["shape"], scale=gammaShape["scale"])
    init$scale <- stats::rgamma(1, shape=gammaScale["shape"], scale=gammaScale["scale"])
  } else {
    init$shape <- ifelse(
      is.na(quantiles["shape"]),
      stats::rgamma(1, shape=gammaShape["shape"], scale=gammaShape["scale"]),
      stats::qgamma(quantiles["shape"], shape=gammaScale["shape"], scale=gammaScale["scale"])
    )
    init$scale <- ifelse(
      is.na(quantiles["scale"]),
      stats::rgamma(1, shape=gammaScale["shape"], scale=gammaScale["scale"]),
      stats::qgamma(quantiles["scale"], shape=gammaScale["shape"], scale=gammaScale["scale"])
    )
  }
  futile.logger::flog.debug("Exit")
  return(init)  
}
