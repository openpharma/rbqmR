test_that("runjags is installed", {
  
  expect_true(requireNamespace("runjags", quietly=TRUE))
  skip_if_not_installed("runjags")
  jagsPath <- runjags::findjags()
  expect_false(jagsPath == "JAGS not found")
  file_test("-f", runjags::findjags())
  file_test("-x", runjags::findjags())
})
