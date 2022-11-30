test_that("createObservedMinusExpectedTable fails gracefully with bad inputs", {
  expect_error(createObservedMinusExpectedTable(NULL))
  expect_error(createObservedMinusExpectedTable(Sys.Date()))
  expect_error(createObservedMinusExpectedTable(tibble::tibble(x=1), timeVar=y))
  expect_error(createObservedMinusExpectedTable(tibble::tibble(x=1), timeVar=x, eventVar=z))
})

test_that("createObservedMinusExpectedTable returns an object of the correct type", {
  rv <- tibble::tibble(
    Subject=1:400,
    Event=rbinom(400, 1, 0.13)
  ) %>% 
  createObservedMinusExpectedTable(
    timeVar = Subject,
    eventVar = Event,
    eventArray = 1,
    expectedRate = 0.1,
    maxTrialSize=400
  ) 
  expect_equal(class(rv), c("tbl_df", "tbl", "data.frame"))
  expect_true(
    length(
      setdiff(
        names(rv), 
        c("Subject", "Event", "SubjectIndex", "CumulativeEvents", "ObservedMinusExpected", 
          "Status", "LowerWarningLimit", "UpperWarningLimit", "LowerActionLimit", 
          "UpperActionLimit"
        )
      )
    ) == 0
  )
  expect_equal(min(rv$LowerActionLimit), -20)
  expect_equal(max(rv$LowerActionLimit), -20)
  expect_equal(min(rv$UpperActionLimit), 20)
  expect_equal(max(rv$UpperActionLimit), 20)
  expect_equal(length(rv$Subject), length(unique(rv$Subject)))
  expect_equal(length(rv$SubjectIndex), length(unique(rv$SubjectIndex)))
  expect_equal(unique(rv$Event), c(0, 1))
  # Monotonicity
  expect_true(all(rv$CumulativeEvents == cummax(rv$CumulativeEvents)))
})