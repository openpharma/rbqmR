test_that("createObservedOverExpectedPlot fails gracefully with bad inputs", {
  expect_error(createObservedOverExpectedPlot(NULL))
  expect_error(createObservedoverExpectedPlot(Sys.Date()))
  expect_error(createObservedOverExpectedPlot(tibble::tibble(x=1), y))
})

test_that("createObservedOverExpectedPlot returns an object of the correct type", {
  observedData <- tibble::tibble(
    NObserved=c(250, 500, 750, 1000), 
    ObservedRate=100*c(2, 9, 15, 16)/NObserved
  )
  
  table <- createObservedOverExpectedTable(
    nHistorical=10000,
    historicalRate=0.014,
    expectedRate=0.014,
    nObservedRange=seq(50, 1500, 25),
    observedData=observedData,
    observedRate=ObservedRate,
    n=NObserved
  )
  
  plot <- table %>% 
            createObservedOverExpectedPlot(observedRate=ObservedRate)
  expect_equal(class(plot), c("gg", "ggplot"))
})