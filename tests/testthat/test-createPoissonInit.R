futile.logger::flog.threshold(futile.logger::FATAL)

test_that(".createBinomialInit handles illegal input correctly", {
  expect_error(.createPoissonInit(gammaShape=c("shape"=2, "bad"=2)))
  expect_error(.createPoissonInit(gammaShape=c("scale"=2, "bad"=2)))
  expect_error(.createPoissonInit(gammaScale=c("shape"=2, "bad"=2)))
  expect_error(.createPoissonInit(gammaScale=c("scale"=-1, "bad"=2)))
  expect_error(.createPoissonInit(gammaShape=c("shape"=-1, "scale"=1)))
  expect_error(.createPoissonInit(gammaShape=c("shape"=-1, "scale"=1)))
  expect_error(.createPoissonInit(gammaScale=c("shape"=-1, "scale"=1)))
  expect_error(.createPoissonInit(gammaScalec("shape"=-1, "scale"=1)))
  suppressWarnings({
    expect_warning(.createPoissonInit(gammaShape=c("shape"=0.5, "scale"=NA)))
    expect_warning(.createPoissonInit(gammaScale=c("shape"=0.5, "scale"=NA)))
  })
  expect_error(.createPoissonInit(seed=3.4))
  expect_error(.createPoissonInit(seed=-2))
  expect_error(.createPoissonInit(quantiles=c("shape"=-0.5, "scale"=NA)))
  expect_error(.createPoissonInit(quantiles=c("shape"=2, "scale"=NA)))
  expect_error(.createPoissonInit(quantiles=c("shape"=0.25, "bad"=0.75)))
})

test_that(".createPoissonInit returns correct RNG", {
  expect_equal(.createPoissonInit()$.RNG.name, "base::Mersenne-Twister")
  expect_equal(.createPoissonInit(rng="base::Mersenne-Twister")$.RNG.name, "base::Mersenne-Twister")
  expect_equal(.createPoissonInit(rng="base::Wichmann-Hill")$.RNG.name, "base::Wichmann-Hill")
  expect_equal(.createPoissonInit(rng="base::Marsaglia-Multicarry")$.RNG.name, "base::Marsaglia-Multicarry")
  expect_equal(.createPoissonInit(rng="base::Super-Duper")$.RNG.name, "base::Super-Duper")
})

test_that(".createPoissonInit handles seed correctly", {
  # Manual seed
  expect_equal(.createPoissonInit(seed=5)$.RNG.seed, 5)
  # Check that 1000 calls provide 1000 different seeds
  set.seed(1)
  expect_equal(length(unique(sapply(1:1000, function(x) .createPoissonInit()$.RNG.seed))), 1000)
})

test_that(".createPoissonInit returns random initial values for lambda", {
  # Check that 1000 calls provide 1000 different lambda
  set.seed(1)
  rv <- sapply(1:1000, function(x) .createPoissonInit()$lambda)
  expect_equal(length(unique(rv)), 1000)
  # Check range of returned values is reasonable
  # expect_true(min(rv) >= 0.001)
  # expect_true(max(rv) <= 0.999)
})

test_that(".createPoissonInit handles quantiles correctly", {
  # Manual quantiles
  q <- c("shape"=0.05, "scale"=0.95)
  rv <- .createPoissonInit(quantiles=q)
  expect_equal(rv$shape, qgamma(q["shape"], shape=1, scale=10))
  expect_equal(rv$scale, qgamma(q["scale"], shape=1, scale=10))
  # Check that 1000 calls provide 1000 different values
  set.seed(1)
  expect_equal(length(unique(sapply(1:1000, function(x) .createPoissonInit()$shape))), 1000)
  expect_equal(length(unique(sapply(1:1000, function(x) .createPoissonInit()$scale))), 1000)
})

test_that(".createPoissonInit returns a list with correctly named elements", {
  expect_true(setequal(names(.createPoissonInit()), c("lambda", "shape", "scale", ".RNG.name", ".RNG.seed")))
})

