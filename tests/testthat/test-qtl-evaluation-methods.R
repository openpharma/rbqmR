testBadValuesToCommonParams <- function(f) {
  expect_error(f(NULL), "data is not a data.frame")
  expect_error(f(1:3), "data is not a data.frame")
  expect_error(f(tibble::tibble(), NULL), "posterior is not a data.frame")
  expect_error(f(tibble::tibble(), 1:3), "posterior is not a data.frame")
}

test_that("evaluateCustomQTL fails gracefully with bad input", {
  testBadValuesToCommonParams(evaluateCustomQTL)
  expect_error(evaluateCustomQTL(tibble::tibble(), tibble::tibble(), NULL), "f cannot be NULL")
  expect_error(evaluateCustomQTL(tibble::tibble(), tibble::tibble(), "notAFunction"), "f is not a function")
  expect_error(evaluateCustomQTL(tibble::tibble(), tibble::tibble(), function() {}), "f does not have at least two arguments")
  expect_error(evaluateCustomQTL(tibble::tibble(), tibble::tibble(), function(a, posterior) {}), "First argument of f is not named 'data'")
  expect_error(evaluateCustomQTL(tibble::tibble(), tibble::tibble(), function(data, b) {}), "Second argument of f is not named 'posterior'")
})

test_that("evaluateCustomQTL works", {
  expect_equal(
    evaluateCustomQTL(tibble::tibble(), tibble::tibble(), function(data, posterior){1}),
    1
  )
})

test_that("evaluatePointEstimateQTL fails gracefully with bad input", {
  testBadValuesToCommonParams(evaluatePointEstimateQTL)
  expect_error(evaluatePointEstimateQTL(tibble::tibble(), tibble::tibble(good=1), observedMetric=bad), "bad is not a column in data")
  # statusCol must not already exist
  expect_error(evaluatePointEstimateQTL(tibble::tibble(good1=1), tibble::tibble(), observedMetric=good1, metric=bad), "bad is not a column in posterior")
  expect_error(evaluatePointEstimateQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), observedMetric=good1, metric=good2, stat=badFunc), "object 'badFunc' not found")
  expect_error(evaluatePointEstimateQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), observedMetric=good1, metric=good2, stat="notAFunction"), "stat is not a function")
  expect_error(evaluatePointEstimateQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), observedMetric=good1, metric=good2, stat=NULL), "stat cannot be NULL")
  expect_error(evaluatePointEstimateQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), observedMetric=good1, metric=good2, stat=median), "Both lower and upper cannot be NULL")
})

test_that("evaluatePointEstimateQTL works with various limit specifications", {
  # Named vector limits
  rv <- berrySummary %>%
    evaluatePointEstimateQTL(
      posterior = tibble::tibble(p=0.6807154),
      metric = p,
      observedMetric = ObservedResponse,
      lower = c("action" = 0.4, "warn" = 0.5),
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
      lower = c("action" = 0.4, "warn" = 0.5),
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
      lower = c(0.4, 0.5, 0.6)
    )
  expect_equal(rv$status, "OK")
  expect_equal(rv$qtl, 0.6807154)
  expect_equal(
    rv$data, 
    berrySummary %>% 
      tibble::add_column(
        Status=c("OK", "2", "OK", "3", "1", "OK", "OK", "OK", "OK")
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

test_that("evaluatePointEstimateQTL works with arbitrary summary functions", {
  # Median
  rv <- berrySummary %>%
          evaluatePointEstimateQTL(
            posterior = tibble::tibble(p=c(0.75, 0.75, 0.85)),
            metric = p,
            stat = median,
            observedMetric = ObservedResponse,
            upper = c("warn" = 0.7, "action" = 0.9)
        )
  expect_equal(rv$status, "warn")
  expect_equal(rv$qtl, 0.75)
  expect_equal(
    rv$data, 
    berrySummary %>% 
      tibble::add_column(
        Status=c("action", "OK", "OK", "OK", "OK", "warn", "warn", "warn", "OK")
      )
  )
})


test_that("evaluateProbabilityInRangeQTL fails gracefully with bad input", {
  testBadValuesToCommonParams(evaluateProbabilityInRangeQTL)
  expect_error(evaluateProbabilityInRangeQTL(tibble::tibble(), tibble::tibble(good=1), bad))
  # statusCol must not already exist
  expect_error(evaluateProbabilityInRangeQTL(tibble::tibble(), tibble::tibble(good1=1, good2=2), good1, statusCol=good2))
  expect_error(evaluateProbabilityInRangeQTL(tibble::tibble(), tibble::tibble(good=1), good, lower=NULL, upper=NULL))
  expect_error(evaluateProbabilityInRangeQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), good2, good1, probs=c(0.5, NA)))
  expect_error(evaluateProbabilityInRangeQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), good2, good1, probs=c(0.5, -1)))
  expect_error(evaluateProbabilityInRangeQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), good2, good1, probs=c(0.5, 2)))
  expect_error(evaluateProbabilityInRangeQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), good2, good1, probs=c(0.5, 0.7), range=c(5, NA)))
})

test_that("evaluateProbabilityInRangeQTL works", {
  rv <- tibble::tibble(
          Site=1:6,
          Metric=seq(0.1, 0.6, 0.1)
        ) %>% 
        evaluateProbabilityInRangeQTL(
          posterior=tibble::tibble(Metric=seq(0.01, 1.0, 0.01)),
          metric=Metric,
          observedMetric=Metric,
          range=c(0.4, 0.7),
          probs=0.5,
          lower=0.25,
          upper=0.55
        )
  expect_equal(rv$status, "action")
  expect_equal(rv$qtl, 0.3)
  expect_equal(
    rv$data %>% dplyr::pull(Status),
    c("action", "action", "OK", "OK", "OK", "action")
  )
})

test_that("evaluateSiteMetricQTL fails gracefully with bad input", {
  testBadValuesToCommonParams(evaluateSiteMetricQTL)
  expect_error(evaluateSiteMetricQTL(tibble::tibble(), tibble::tibble(good=1), bad))
  # statusCol must not already exist
  expect_error(evaluateSiteMetricQTL(tibble::tibble(), tibble::tibble(good1=1, good2=2), good1, statusCol=good2))
  expect_error(evaluateSiteMetricQTL(tibble::tibble(), tibble::tibble(good=1), good, lower=NULL, upper=NULL))
  expect_error(evaluateSiteMetricQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), good2, good1))
  expect_error(evaluateSiteMetricQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), good2, good1, statusFunc=NULL))
  expect_error(evaluateSiteMetricQTL(tibble::tibble(good1=1), tibble::tibble(good2=1), good2, good1, statusFunc=4.3))
})

test_that("evaluateSiteMetricQTL works", {
  fitted <- berrySummary %>%
              fitBayesBinomialModel(n = Subjects, r = Events)
  rv <- berrySummary %>%
          evaluateSiteMetricQTL(
          posterior=fitted$tab,
          metric=p,
          observedMetric=ObservedResponse,
          lower=c("action"=0.5, "warn"=0.6),
          upper=c("action"=0.9, "warn"=0.8),
          statusFunc=function(d) ifelse(
            d %>%
              dplyr::filter(Status == "action") %>%
              dplyr::pull(N) > 1, "action", "OK"
          )
        )
  expect_equal(rv$status, "action")
  # expect_equal(rv$qtl, tibble::tibble(Status=c("OK", "action", "warn"), N=c(5, 2, 2)))
  # expect_equal(
  #   rv$data %>% dplyr::pull(Status),
  #   c("action", "warn", "OK", "warn", "action", "OK", "warn", "OK", "OK")
  # )
  # expect_equal(
  #   rv$quantiles %>% dplyr::mutate(p=round(p, 2)),
  #   tibble::tibble(
  #     Status=c("action", "warn", "warn", "action"),
  #     p=c(0.70, 0.74, 0.83, 0.89)
  #   )
  # )
})
  