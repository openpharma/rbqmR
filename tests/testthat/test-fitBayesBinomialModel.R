test_that("Issue #3 has been resolved", {
  expect_error(fitBayesBinomialModel(data=NULL), NA)
})

test_that("Issue #4 has been resolved", {
  inits <- lapply(1:3, function(x) rbqmR:::.createBinomialInit())
  explicitPrior <- fitBayesBinomialModel(
    data=NULL, 
    prior=getModelString("binomial", prior=TRUE), 
    inits=inits
  )  
  implicitPrior <- fitBayesBinomialModel(
    data=NULL, 
    inits=inits
  )
  expect_identical(implicitPrior$tab, explicitPrior$tab)
})

