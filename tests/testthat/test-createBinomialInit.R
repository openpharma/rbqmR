logger::log_threshold(logger::FATAL)

test_that(".createBinomialInit handles illegal input correctly", {
  expect_error(.createBinomialInit(gammaA=c("shape"=2, "bad"=2)))
  expect_error(.createBinomialInit(gammaA=c("scale"=2, "bad"=2)))
  expect_error(.createBinomialInit(gammaB=c("shape"=2, "bad"=2)))
  expect_error(.createBinomialInit(gammaB=c("scale"=-1, "bad"=2)))
  expect_error(.createBinomialInit(gammaA=c("shape"=-1, "scale"=1)))
  expect_error(.createBinomialInit(gammaA=c("shape"=-1, "scale"=1)))
  expect_error(.createBinomialInit(gammaB=c("shape"=-1, "scale"=1)))
  expect_error(.createBinomialInit(gammaB=c("shape"=-1, "scale"=1)))
  suppressWarnings({
    expect_warning(.createBinomialInit(gammaA=c("shape"=0.5, "scale"=NA)))
    expect_warning(.createBinomialInit(gammaB=c("shape"=0.5, "scale"=NA)))
  })
  expect_error(.createBinomialInit(seed=3.4))
  expect_error(.createBinomialInit(seed=-2))
  expect_error(.createBinomialInit(quantiles=c("a"=-0.5, "b"=NA)))
  expect_error(.createBinomialInit(quantiles=c("a"=2, "b"=NA)))
  expect_error(.createBinomialInit(quantiles=c("a"=0.25, "bad"=0.75)))
})

test_that(".createBinomialInit returns correct RNG", {
  expect_equal(.createBinomialInit()$RNG.name, "base::Mersenne-Twister")
  expect_equal(.createBinomialInit(rng="base::Mersenne-Twister")$RNG.name, "base::Mersenne-Twister")
  expect_equal(.createBinomialInit(rng="base::Wichmann-Hill")$RNG.name, "base::Wichmann-Hill")
  expect_equal(.createBinomialInit(rng="base::Marsaglia-Multicarry")$RNG.name, "base::Marsaglia-Multicarry")
  expect_equal(.createBinomialInit(rng="base::Super-Duper")$RNG.name, "base::Super-Duper")
})

test_that(".createBinomialInit handles seed correctly", {
  # Manual seed
  expect_equal(.createBinomialInit(seed=5)$RNG.seed, 5)
  # Check that 1000 calls provide 1000 different seeds
  set.seed(1)
  expect_equal(length(unique(sapply(1:1000, function(x) .createBinomialInit()$RNG.seed))), 1000)
})

test_that(".createBinomialInit returns random initial values for p", {
  # Check that 1000 calls provide 1000 different p
  set.seed(1)
  rv <- sapply(1:1000, function(x) .createBinomialInit()$p)
  expect_equal(length(unique(rv)), 1000)
  # Check range of returned values is reasonable
  expect_true(min(rv) >= 0.001)
  expect_true(max(rv) <= 0.999)
})

test_that(".createBinomialInit handles quantiles correctly", {
  # Manual quantiles
  q <- c("a"=0.05, "b"=0.95)
  rv <- .createBinomialInit(quantiles=q)
  expect_equal(rv$a, qgamma(q["a"], shape=1, scale=10))
  expect_equal(rv$b, qgamma(q["b"], shape=1, scale=10))
  # Check that 1000 calls provide 1000 different values
  set.seed(1)
  expect_equal(length(unique(sapply(1:1000, function(x) .createBinomialInit()$a))), 1000)
  expect_equal(length(unique(sapply(1:1000, function(x) .createBinomialInit()$b))), 1000)
})

test_that(".createBinomialInits returns a list with correctly named elements", {
  expect_true(setequal(names(.createBinomialInit()), c("p", "a", "b", "RNG.name", "RNG.seed")))
})

