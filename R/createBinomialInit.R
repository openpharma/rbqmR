#' Create a JAGS inits suitable for use with run.jags and autorun.jags
#'
#' Initial values of p are chosen uniformly at random in the range 0.001 to 
#' 0.999 inclusive.
#'
#' @param gammaA a vector with elements named shape and scale that define the
#' hyper prior for `a` in the Beta prior for `p`.
#' @param gammaB a vector with elements named shape and scale that define the
#' hyper prior for `b` in the Beta prior for `p`.
#' @param seed a scalar containing the seed to be used for the init.  `NULL`,
#' the default, indicates that a random  seed should be used.  It
#' is not clear from the runjags or JAGS manuals what the range of permissible
#' seed values is, but positive integers appear to be acceptable.
#' @param quantiles Default `NULL`. If `NULL`, starting values of `a` and
#' `b` are chosen at random.  Otherwise, starting values are defined by the 
#' values supplied in `quantiles`. See Usage Notes below.
#' @param rng the random number generator to be used for each init
#' @section Usage Notes:
#' The `quantiles` parameter provides a flexible way of defining starting values
#' for the hyperparameters `a` and `b`.  If `NULL`, the default, starting values
#' are random values from either `gammaA` (for `a`) or `gammaB` (for `b`).  
#' Otherwise, `quantiles` should be a numeric vector of length 2 with values
#' (named `a` and `b`) that are both either `NA` or in the range (0, 1).  If 
#' `NA`, the corresponding staring value is chosen at random, as above.  Otherwise,
#' if the value is `x`, the starting value of the hyperparameter is the `x`th
#' quantile of the corresponding Gamma distribution.
#' 
#' For example, if `quantiles=c("a"=NA, "b"=50)`, then the starting value for `a`
#' is a random value drawn from the distribution defined by `gammaA` and the
#' starting value of `b` is the 50th centile of the distribution defined by
#' `gammaB`. 
#' @return The init: a list with elements named `p`, `a`, `b`, `RNG.name` and
#'  `RNG.seed`.
.createBinomialInit <- function(
    gammaA=c(shape=1, scale=10),
    gammaB=c(shape=1, scale=10),
    seed=NULL,
    quantiles=NULL,
    rng = c(
            "base::Mersenne-Twister", 
            "base::Wichmann-Hill", 
            "base::Marsaglia-Multicarry", 
            "base::Super-Duper"
          )
    ) {
  logger::log_debug("Entry")
  logger::log_trace(paste0("  gammaA - shape: ", gammaA["shape"], ", scale: ", gammaA["scale"]))
  logger::log_trace(paste0("  gammaB - shape: ", gammaB["shape"], ", scale: ", gammaB["scale"]))
  if (is.null(seed)) {
    logger::log_trace("  seed: NULL")
  } else {
    logger::log_trace(paste0("  seed: ", seed))
  }
  if (is.null(quantiles)) {
    logger::log_trace("  quantuiles: NULL")
  } else {
    logger::log_trace(paste0("  quantiles - a: ", quantiles["a"], "; b: ", quantiles[["b"]]))
  }
  logger::log_trace(paste0("  rng: ", rng))

  # # Validate
  if (!setequal(names(gammaA), c("shape", "scale"))) stop("elements of gammaA are not named 'shape' and 'scale'.")
  if (!setequal(names(gammaB), c("shape", "scale"))) stop("elements of gammaB are not named 'shape' and 'scale'.")
  if (!is.null(quantiles)) {
    if (!setequal(names(quantiles), c("a", "b"))) stop("elements of quantiles are not named 'a' and 'b'.")
    if (max(quantiles, na.rm=TRUE) >= 1) stop("not all elements of quantiles are less than 1.")
    if (min(quantiles, na.rm=TRUE) <= 0) stop("not all elements of quantiles are greater than 0.")
  }
  if (min(gammaA, na.rm=TRUE) <= 0) stop("not all elements of gammaA are greater than 0.")
  if (min(gammaB, na.rm=TRUE) <= 0) stop("not all elements of gammaB are greater than 0.")
  
  if (gammaA["shape"] < 1 | gammaA["scale"] < 1) {
    logger::log_warn("At least one element of gammaA is less than 1.  It is recommended that both elements are greater than 1 so that the hyperprior is not U-shaped.")
    warning("At least one element of gammaA is less than 1.  It is recommended that both elements are greater than 1 so that the hyperprior is not U-shaped.")
  }
  if (gammaB["shape"] < 1 | gammaB["scale"] < 1) {
    logger::log_warn("At least one element of gammaB is less than 1.  It is recommended that both elements are greater than 1 so that the hyperprior is not U-shaped.")
    warning("At least one element of gammaB is less than 1.  It is recommended that both elements are greater than 1 so that the hyperprior is not U-shaped.")
  }
  if (!is.null(seed)) {
    if (seed < 0 | seed != as.integer(seed)) stop("Seed must be a positive integer")
  }
  if (!is.null(quantiles)) {
    if (min(quantiles, na.rm=TRUE) <= 0) stop("not all elements of quantiles are greater than 0.")
    if (max(quantiles, na.rm=TRUE) >= 1) stop("not all elements of quantiles are less than 1.")
    if (!setequal(names(quantiles), c("a", "b"))) stop("elements of quantiles are not named 'a' and 'b'.")
  }
  # Execute
  rng <- match.arg(rng)
  init <- list(
            "RNG.name"=rng, 
            "RNG.seed"=ifelse(is.null(seed), stats::runif(1, max=.Machine$integer.max), seed),
            "p"=stats::runif(1, min=0.001, max=0.999)
  )
  if (is.null(quantiles)) {
    init$a <- stats::rgamma(1, shape=gammaA["shape"], scale=gammaA["scale"])
    init$b <- stats::rgamma(1, shape=gammaB["shape"], scale=gammaB["scale"])
  } else {
    init$a <- ifelse(
                is.na(quantiles["a"]), 
                stats::rgamma(1, shape=gammaA["shape"], scale=gammaA["scale"]),
                stats::qgamma(quantiles["a"], shape=gammaA["shape"], scale=gammaA["scale"])
              )
    init$b <- ifelse(
      is.na(quantiles["b"]), 
      stats::rgamma(1, shape=gammaB["shape"], scale=gammaB["scale"]),
      stats::qgamma(quantiles["b"], shape=gammaB["shape"], scale=gammaB["scale"])
    )
  }
  logger::log_debug("Exit")
  return(init)
}