#' Create a JAGS inits suitable for use with run.jags and autorun.jags
#'
#' @param quantiles Default `NULL`. If `NULL`, starting values of `a` and
#' `b` are chosen at random.  Otherwise, starting values are defined by the 
#' values supplied in `quantiles`. See Usage Notes below.
#' @param rng the random number generator to be used for each init
#' @param n the length of the initialisation vector for `p`
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
  # Execute
  init <- list(
    ".RNG.name"=rng, 
    ".RNG.seed"=ifelse(is.null(seed), stats::runif(1, max=.Machine$integer.max), seed),
    "lambda"=stats::rgamma(n, shape=1, scale=1)
  )
  if (is.null(quantiles)) {
    # init$a <- stats::rgamma(1, shape=gammaA["shape"], scale=gammaA["scale"])
    # init$b <- 1/stats::rgamma(1, shape=gammaB["shape"], scale=gammaB["scale"])
    init$shape <- stats::rgamma(1, shape=gammaShape["shape"], scale=gammaShape["scale"])
    init$scale <- stats::rgamma(1, shape=gammaScale["shape"], scale=gammaScale["scale"])
  } else {
    init$a <- ifelse(
      is.na(quantiles["shape"]),
      stats::rgamma(1, shape=gammaShape["shape"], scale=gammaShape["scale"]),
      stats::qgamma(quantiles["shape"], shape=gammaScale["shape"], scale=gammaScale["scale"])
    )
    init$b <- ifelse(
      is.na(quantiles["scale"]),
      1/stats::rgamma(1, shape=gammaScale["shape"], scale=gammaScale["scale"]),
      1/stats::qgamma(quantiles["scale"], shape=gammaScale["shape"], scale=gammaScale["scale"])
    )
  }
  logger::log_debug("Exit")
  return(init)  
}
