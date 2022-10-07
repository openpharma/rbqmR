test_that("test that getModelString works with default hyperParameters", {
  expect_equal(
    stringr::str_squish(getModelString("binomial")), 
    "model { for (i in 1:k) { r[i] ~ dbin(p[i], n[i]) p[i] ~ dbeta(a, b) } a ~ dunif(0, 10) b ~ dunif(0, 10) }"
  )
  expect_equal(
    stringr::str_squish(getModelString("binary")), 
    "model { for (j in 1:m) { p[j] ~ dbeta(a, b) } for (i in 1:k) { r[i] ~ dbern(p[group[i]]) } a ~ dunif(0, 10) b ~ dunif(0, 10) }"
  )
  expect_equal(
    stringr::str_squish(getModelString("poisson")), 
    "model { for (i in 1:k) { events[i] ~ dpois(mu[i]) mu[i] <- lambda[i]*exposure[i] lambda[i] ~ dgamma(shape, 1/scale) } scale ~ dgamma(1, 1) shape ~ dgamma(1, 1) }"
  )
  expect_equal(
    stringr::str_squish(getModelString("normal")), 
    "model { #Likelihood for (i in 1:n) { x[i] ~ dnorm(mu[g[i]], tau) } #Prior for (i in 1:k) { mu[i] ~ dnorm(mu0, tau) } mu0 ~ dnorm(0, 1e-06) invTau ~ dgamma(1e-06, 1e-06) #Transform tau <- 1/invTau # Because inverse gamma isn't directly supported }"
  )  
  expect_equal(
    stringr::str_squish(getModelString("tte")), 
    "model { #Likelihood for (i in 1:k) { logMean[i] ~ dnorm(mu[i], n[i] / tau) } #Prior for (i in 1:k) { mu[i] ~ dnorm(mu0, tau) } mu0 ~ dnorm(0, 1e-06) invTau ~ dgamma(1e-06, 1e-06) #Transform tau <- 1/invTau # Because inverse gamma isn't directly supported }"
  )
})

test_that("test that getModelString works with hyperParameters as vector", {
  expect_equal(
    stringr::str_squish(getModelString("binomial", c("<priorA>"="dexp(1)", "<priorB>"="dunif(10, 20)"))), 
    "model { for (i in 1:k) { r[i] ~ dbin(p[i], n[i]) p[i] ~ dbeta(a, b) } a ~ dexp(1) b ~ dunif(10, 20) }"
  )
})

test_that("test that getModelString fails gracefully", {
  logger::log_threshold(logger::FATAL)
  suppressWarnings({
    expect_warning(rv <- getModelString("binomial", list()))
    expect_equal(
      stringr::str_squish(rv),
      "model { for (i in 1:k) { r[i] ~ dbin(p[i], n[i]) p[i] ~ dbeta(a, b) } a ~ <priorA> b ~ <priorB> }"
    )
  })
})
