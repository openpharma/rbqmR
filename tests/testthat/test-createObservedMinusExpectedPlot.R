test_that("createObservedMinusExpectedPlot fails gracefully with bad inputs", {
  expect_error(createObservedMinusExpectedPlot(NULL))
  expect_error(createObservedMinusExpectedPlot(Sys.Date()))
  expect_error(createObservedMinusExpectedPlot(tibble::tibble(x=1), y))
})

test_that("createObservedMinusExpectedPlot returns an object of the correct type", {
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
        ) %>%
        createObservedMinusExpectedPlot()
  expect_equal(class(rv), c("gg", "ggplot"))
})