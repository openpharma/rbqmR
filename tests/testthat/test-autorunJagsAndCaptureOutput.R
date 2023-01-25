test_that(".autorunJagsAndCaptureOutput fails gracefully with bad inputs", {
  futile.logger::flog.threshold(futile.logger::FATAL)
  expect_error(.autorunJagsAndCaptureOutput(NULL), "data is not a data.frame")
  expect_error(.autorunJagsAndCaptureOutput(Sys.Date()), "data is not a data.frame")
  expect_error(.autorunJagsAndCaptureOutput(tibble::tibble(), inits=NULL), "inits cannot be NULL")
  
  init1 <- .createBinomialInit()
  init2 <- .createBinomialInit()
  expect_equal(
    (berrySummary %>% .autorunJagsAndCaptureOutput(model="badModelString", inits=list(init1, init2)))$status, 
    "ERROR"
  )
})
