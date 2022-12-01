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

test_that("evaluatePointEstimateQTL works", {
  # Named vector limits
  rv <- berrySummary %>%
    evaluatePointEstimateQTL(
      posterior = tibble::tibble(p=0.6807154),
      metric = p,
      observedMetric = ObservedResponse,
      lower = c("warn" = 0.5, "action" = 0.4),
      upper = c("warn" = 0.8, "action" = 0.9)
    )
  expect_equal(rv$status, "OK")
  expect_equal(rv$qtl, 0.6807154)
  expect_equal(
    rv$data, 
    berrySummary %>% 
      tibble::add_column(
        Status=c("action", "warn", "OK", "OK", "action", "OK", "warn", "OK", "OK")
      )
  )
  # Unnamed vector limits, size 2
  rv <- berrySummary %>%
    evaluatePointEstimateQTL(
      posterior = tibble::tibble(p=0.6807154),
      metric = p,
      observedMetric = ObservedResponse,
      lower = c("warn" = 0.5, "action" = 0.4),
      upper = c("warn" = 0.8, "action" = 0.9)
    )
  expect_equal(rv$status, "OK")
  expect_equal(rv$qtl, 0.6807154)
  expect_equal(
    rv$data, 
    berrySummary %>% 
      tibble::add_column(
        Status=c("action", "warn", "OK", "OK", "action", "OK", "warn", "OK", "OK")
      )
  )
  # Unnamed vector limits, size 3
  rv <- berrySummary %>%
    evaluatePointEstimateQTL(
      posterior = tibble::tibble(p=0.6807154),
      metric = p,
      observedMetric = ObservedResponse,
      lower = c(0.6, 0.5, 0.4)
    )
  expect_equal(rv$status, "OK")
  expect_equal(rv$qtl, 0.6807154)
  expect_equal(
    rv$data, 
    berrySummary %>% 
      tibble::add_column(
        Status=c("OK", "2", "OK", "1", "3", "OK", "OK", "OK", "OK")
      )
  )
  # Scalar limits
  rv <- berrySummary %>%
    evaluatePointEstimateQTL(
      posterior = tibble::tibble(p=0.6807154),
      metric = p,
      observedMetric = ObservedResponse,
      lower = 0.39,
      upper = 0.89
    )
  expect_equal(rv$status, "OK")
  expect_equal(rv$qtl, 0.6807154)
  expect_equal(
    rv$data, 
    berrySummary %>% 
      tibble::add_column(
        Status=c("action", "OK", "OK", "OK", "action", "OK", "action", "OK", "OK")
      )
  )
})
