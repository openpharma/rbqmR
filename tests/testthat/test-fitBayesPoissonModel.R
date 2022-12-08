logger::log_threshold(logger::FATAL)

test_that("Issue #3 has been resolved", {
  rv <- fitBayesPoissonModel(data=NULL)
  expect_true(rv$status == "OK")
  print(runjags::failed.jags('inits'))
  print(rv)
})

test_that("Issue #4 has been resolved", {
  inits <- lapply(1:2, function(x) rbqmR:::.createPoissonInit())
  inits
  explicitPrior <- fitBayesPoissonModel(
    data=NULL,
    prior=getModelString("poisson", prior=TRUE),
    inits=inits
  )
  implicitPrior <- fitBayesPoissonModel(
    data=NULL,
    inits=inits
  )
  expect_identical(implicitPrior$tab, explicitPrior$tab)
})

test_that("fitBayesPoissonModel fails gracefully with bad inputs", {
  expect_error(tibble(n=c(1, NA), r=c(0, 0)) %>% fitBayesPoissonModel(n, r))
  expect_error(tibble(n=c(1, 1), r=c(0, NA)) %>% fitBayesPoissonModel(n, r))
  expect_error(tibble(n=c(1, 0), r=c(0, 0)) %>% fitBayesPoissonModel(n, r))
  expect_error(tibble(n=c(1, 1), r=c(0, -1)) %>% fitBayesPoissonModel(n, r))
  expect_error(tibble(n=c(1, 1.5), r=c(0, 0)) %>% fitBayesPoissonModel(n, r))
  expect_error(tibble(n=c(1, 1.5), r=c(0, 0.5)) %>% fitBayesPoissonModel(n, r))
  expect_error(tibble(n=c(1, 1), r=c(0, 2)) %>% fitBayesPoissonModel(n, r))
})

test_that("fitBayesPoissonModel produces feasible results", {
  data("cavalryDeaths")
  rv <- cavalryDeaths %>%
          dplyr::summarise(Deaths=sum(Deaths), Years=sum(Year)) %>%
          fitBayesPoissonModel(Deaths, Years)
  expect_equal(rv$status, "OK")
  
  
  
  
  # expect_equal(
  #   rv$tab %>% dplyr::summarise(mean=round(mean(lambda), 7)),
  #   tibble::tibble(mean=0.0003929)
  # )
  # Number of MCMC samples
  # expect_true(rv$tab %>% nrow() >= 20000)
})