test_that("createObservedOverExpectedTable fails gracefully with bad inputs", {
  expect_error(createObservedOverExpectedTable(Sys.Date()))
  expect_error(createObservedOverExpectedTable(tibble::tibble(x=1), observedRate=y))
})

test_that("createObservedOverExpectedTable returns an object of the correct type", {
  rv <- createObservedOverExpectedTable(
          tibble::tibble(NObserved=seq(50, 400, 50)),
          observedRate=NObserved,
          nHistorical=10000,
          historicalRate=0.014,
          expectedRate=0.014,
          nObservedRange=seq(50, 1500, 25)
        )
  expect_equal(class(rv), c("tbl_df", "tbl", "data.frame"))
  expect_true(
    length(
      setdiff(
        names(rv),
        c("NObserved", "NHistorical", "ExpectedRate", "HistoricalRate", "Alpha", 
          "Sides", "Ratio", "Lower", "Upper")
      )
    ) == 0
  )
  expect_equal(min(rv$NHistorical), 10000)
  expect_equal(max(rv$NHistorical), 10000)
  expect_equal(min(rv$ExpectedRate), 0.014)
  expect_equal(max(rv$ExpectedRate), 0.014)
  expect_equal(min(rv$HistoricalRate), 0.014)
  expect_equal(max(rv$HistoricalRate), 0.014)
  expect_equal(min(rv$Alpha), 0.05)
  expect_equal(max(rv$Alpha), 0.05)
  expect_true(all(rv$Sides == "two"))
  expect_true(all(rv$Ratio == 1))
  # Monotonicity
  expect_true(all(rv$Lower == cummax(rv$Lower)))
  expect_true(all(rv$Upper == cummin(rv$Upper)))
})