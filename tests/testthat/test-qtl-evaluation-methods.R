testBadValuesToCommonParams <- function(f) {
  expect_error(f(NULL))
  expect_error(f(1:3))
  expect_error(f(tibble::tibble(), NULL))
  expect_error(f(tibble::tibble(), 1:3))
  expect_error(f(tibble::tibble(), tibble::tibble(), NULL))
  expect_error(f(tibble::tibble(), tibble::tibble(), function(){}))
  expect_error(f(tibble::tibble(), tibble::tibble(), function(a, posterior){}))
  expect_error(f(tibble::tibble(), tibble::tibble(), function(data, b){}))
}

test_that("evaluateCustomQTL fails gracefully with bad input", {
  testBadValuesToCommonParams(evaluateCustomQTL)
})

test_that("evaluateCustomQTL works", {
  expect_equal(
    evaluateCustomQTL(tibble::tibble(), tibble::tibble(), function(data, posterior){1}),
    1
  )
})

test_that("evaluatePointEstimateQTL fails gracefully with bad input", {
  testBadValuesToCommonParams(evaluatePointEstimateQTL)
  expect_error(evaluatePointEstimateQTL(tibble::tibble(), tibble::tibble(good=1), bad))
  expect_error(evaluatePointEstimateQTL(tibble::tibble(), tibble::tibble(good=1), good, badFunc))
  expect_error(evaluatePointEstimateQTL(tibble::tibble(), tibble::tibble(good=1), good, median))
  expect_error(evaluatePointEstimateQTL(tibble::tibble(), tibble::tibble(good=1), good, median, NULL, NULL))
})
