test_that(".autorunJagsAndCaptureOutput fails gracefully with bad inputs", {
  futile.logger::flog.threshold(futile.logger::FATAL)
  expect_error(.autorunJagsAndCaptureOutput(NULL))
  expect_error(.autorunJagsAndCaptureOutput(Sys.Date()))
  expect_error(.autorunJagsAndCaptureOutput(tibble::tibble(), inits=NULL))
})
