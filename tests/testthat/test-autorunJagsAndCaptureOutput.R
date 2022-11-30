test_that(".autorunJagsAndCaptureOutput fails gracefully with bad inputs", {
  logger::log_threshold(logger::FATAL)
  expect_error(.autorunJagsAndCaptureOutput(NULL))
  expect_error(.autorunJagsAndCaptureOutput(Sys.Date()))
  expect_error(.autorunJagsAndCaptureOutput(tibble::tibble(), inits=NULL))
})
