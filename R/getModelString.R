#' Function to obtain the string that defines the default JAGS model for each
#' data type
#'
#' @param label the type of model
#' @param hyperParams a named list or named vector that defines how the 
#' placeholders in the model string are to be handled.  See Usage Notes below.
#' @return a string defining the required model
#' @param prior Boolean.  Should the model for the prior be returned.  Default:
#' `FALSE`.
#' @section Usage Notes:
#' If `hyperParams` is a list, then the placeholder replacements are taken from
#' `hyperParams[[label]]`, which should be a named vector.  Otherwise, the 
#' placeholder replacements are taken from `hyperParams` directly.  Again, 
#' `hyperParams` should be a named vector.  Once a named vector has been 
#' obtained from `hyperParams`, names of `hyperParams` define the placeholders
#' and the replacements by the corresponding values.
#' 
#' For example, `hyperParams` has the value `c("<priorA>"="dunif(0, 10)",`
#' `"<priorB>"="dunif(0, 20)")`, then where ever `<priorA> is found in the model
#' string, it is replaced by `dunif(0, 10)`.  Similarly, all occurrences of
#' `<priorB>` will be replaced by `dunif(0, 20)`.
#' 
#' The default value of `hyperParams` provides the default hyperparameter values
#' used by `rbqmR`'s Bayes model fitting functions.
#' @examples
#' #To fit a binary model
#' binModel <- getModelString("binary")
#' @section Usage notes:
#' \code{label} is determined case insensitively
#' @export
getModelString <- function(
                    label=c("binary", "binomial", "normal", "poisson", "tte"), 
                    hyperParams=list(
                                  "binomial"=c(
                                     "<priorA>"="dunif(0, 10)",
                                     "<priorB>"="dunif(0, 10)"
                                  ),
                                  "binary"=c(
                                    "<priorA>"="dunif(0, 10)",
                                    "<priorB>"="dunif(0, 10)"
                                  ),
                                  "normal"=c(
                                    "<priorMu0>"="dnorm(0, 1e-06)",
                                    "<priorInvTau>"="dgamma(1e-06, 1e-06)"
                                  ),
                                  "poisson"=c(
                                    "<priorShape>"="dgamma(1, 1)",
                                    "<priorScale>"="dgamma(1, 1)"
                                  ),
                                  "tte"=c(
                                    "<priorMu0>"="dnorm(0, 1e-06)",
                                    "<priorInvTau>"="dgamma(1e-06, 1e-06)"
                                  )
                               ),
                    prior=FALSE
                  ) {
  logger::log_debug("Entry")
  label <- match.arg(label)
  s <- ""
  s <- switch(label,
              binary=
"model {
   for (j in 1:m) {
     p[j] ~ dbeta(a, b)
   }
   <notprior>
   for (i in 1:k) {
     r[i] ~ dbern(p[group[i]])
   }
   </notprior>
   a ~ <priorA>
   b ~ <priorB>
}",
              binomial=
"model {
   for (i in 1:k) {
    <notprior>
    r[i] ~ dbin(p[i], n[i])
    </notprior>
    p[i] ~ dbeta(a, b)
  }
  a ~ <priorA>
  b ~ <priorB>
}",
              poisson=
"model {
   for (i in 1:k) {
     <notprior>
     events[i] ~ dpois(mu[i])
     mu[i] <- lambda[i]*exposure[i]
     </notprior>
     lambda[i] ~ dgamma(shape, 1/scale)
   }
   scale ~ <priorScale>
   shape ~ <priorShape>
 }",
              tte=
"model {
   #Likelihood
   <notprior>
   for (i in 1:k) {
     logMean[i] ~ dnorm(mu[i], n[i] / tau)
   }
   </notprior>
   #Prior
   for (i in 1:k) {
     mu[i] ~ dnorm(mu0, tau)
   }
   mu0 ~ <priorMu0>
   invTau ~ <priorInvTau>
   #Transform
   tau <- 1/invTau  # Because inverse gamma isn't directly supported
}",
              normal=
"model {
   <notprior>
   #Likelihood
   for (i in 1:n) {
     x[i] ~ dnorm(mu[g[i]], tau)
   }
   </notprior>
   #Prior
   for (i in 1:k) {
     mu[i] ~ dnorm(mu0, tau)
   }
   mu0 ~ <priorMu0>
   invTau ~ <priorInvTau>
   #Transform
   tau <- 1/invTau  # Because inverse gamma isn't directly supported
}")
  if (length(s) == 0) stop(paste0("Unsupported data type [", label, "]."))
  # Replace placeholders with requested strings
  if (is.list(hyperParams)) {
    if (!(label %in% names(hyperParams))) {
       msg <- paste0("hyperParameter list does not contain an entry named `", label, "`.  No replacement of placeholders will occur.")
      logger::log_warn(msg)
      warning(msg)
    }
    hyperParams <- hyperParams[[label]]
  }
  if (!is.null(hyperParams)) {
    s <- stringr::str_replace_all(s, hyperParams)
  }
  if (prior) {
    s <- s %>% stringr::str_replace_all("<notprior>(.|\\n)*<\\/notprior>", "")
    #s <- s %>% stringr::str_replace_all("\\n\\s*\\n", "\\n")
  } else {
    s <- s %>% stringr::str_replace_all("<[\\/]*notprior>", "")
  }
  logger::log_debug("Exit")
  return (s)
}

getModelString("binomial", c("<priorA>"="dnorm()", "<priorB>"="dexp(5)"))
