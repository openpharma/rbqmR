futile.logger::flog.threshold(futile.logger::FATAL)

test_that("Issue #3 has been resolved", {
  rv <- fitBayesPoissonModel(data=NULL)
  expect_true(rv$status == "OK")
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
  expect_error(tibble::tibble(events=c(1, NA), exposure=c(0, 0)) %>% fitBayesPoissonModel(events, exposure), "Some entries in events are NA")
  expect_error(tibble::tibble(events=c(1, 1), exposure=c(0, NA)) %>% fitBayesPoissonModel(events, exposure), "Some entries in exposure are NA")
  expect_error(tibble::tibble(events=c(1, 1), exposure=c(0, -1)) %>% fitBayesPoissonModel(events, exposure), "Not all entries in exposure are non-negative")
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