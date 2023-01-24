futile.logger::flog.threshold(futile.logger::FATAL)

test_that("Issue #3 has been resolved", {
  expect_error(fitBayesBinomialModel(data=NULL), NA)
})

test_that("Issue #4 has been resolved", {
  inits <- lapply(1:2, function(x) rbqmR:::.createBinomialInit())
  explicitPrior <- fitBayesBinomialModel(
    data=NULL, 
    model=getModelString("binomial", prior=TRUE), 
    inits=inits
  )  
  implicitPrior <- fitBayesBinomialModel(
    data=NULL, 
    inits=inits
  )
  expect_identical(implicitPrior$tab, explicitPrior$tab)
})

test_that("fitBayesBinomialModel fails gracefully with bad inputs", {
  expect_error(tibble(n=c(1, NA), r=c(0, 0)) %>% fitBayesBinomialModel(n, r))
  expect_error(tibble(n=c(1, 1), r=c(0, NA)) %>% fitBayesBinomialModel(n, r))
  expect_error(tibble(n=c(1, 0), r=c(0, 0)) %>% fitBayesBinomialModel(n, r))
  expect_error(tibble(n=c(1, 1), r=c(0, -1)) %>% fitBayesBinomialModel(n, r))
  expect_error(tibble(n=c(1, 1.5), r=c(0, 0)) %>% fitBayesBinomialModel(n, r))
  expect_error(tibble(n=c(1, 1.5), r=c(0, 0.5)) %>% fitBayesBinomialModel(n, r))
  expect_error(tibble(n=c(1, 1), r=c(0, 2)) %>% fitBayesBinomialModel(n, r))
  expect_error(tibble(n=c(1, 1), r=c(0, 0)) %>% fitBayesBinomialModel(n, r, inits=list(list("bad"=1))))
})

test_that("fitBayesBinomialModel reproduces results from Berry et al", {
  data("berrySummary")
  rv <- berrySummary %>% fitBayesBinomialModel(Subjects, Events)
  expect_equal(rv$status, "OK")
  # Table 2.2 page 63
  expect_equal(
    rv$tab %>% dplyr::summarise(mean=round(mean(p), 2)), 
    tibble::tibble(mean=0.68)
  )
})