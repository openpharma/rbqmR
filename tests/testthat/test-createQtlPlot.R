test_that("createQtlPlot fails gracefully with bad input", {
  expect_error(createQtlPlot(NULL))
  expect_error(createQtlPlot(tibble::tibble(good=1), bad))
  expect_error(createQtlPlot(tibble::tibble(good=1), good, siteData=tibble(good=1), siteMetric=bad))
  expect_error(createQtlPlot(tibble::tibble(good=1), good, siteData=tibble(good=1), siteSize=bad))
  expect_error(createQtlPlot(Sys.Date()))
})

test_that("createQtlPlot creates a valid ggplot object", {
  fitted <- (berrySummary %>% fitBayesBinomialModel(Subjects, Events))$tab
  
  quantiles <- fitted %>% 
    dplyr::summarise(
      Q05=quantile(p, probs=0.05, names=FALSE),
      Q20=quantile(p, probs=0.20, names=FALSE),
      Q80=quantile(p, probs=0.80, names=FALSE),
      Q95=quantile(p, probs=0.95, names=FALSE)
    )
  
  rv <- fitted %>% 
    createQtlPlot(
      siteData=berrySummary,
      siteMetric=ObservedResponse,
      siteSize=Subjects,
      observedMetric=fitted %>% dplyr::summarise(Mean=mean(p)) %>% dplyr::pull(Mean),
      actionLimits=list(
        list("lower"=quantiles$Q95, "upper"=NA, "alpha"=0.6, "colour"="goldenrod1"),
        list("lower"=NA, "upper"=quantiles$Q05, "alpha"=0.6, "colour"="goldenrod1")
      ),
      warningLimits=list(
        list("lower"=quantiles$Q80, "upper"=quantiles$Q95, "alpha"=0.2, "colour"="goldenrod1"),
        list("lower"=quantiles$Q05, "upper"=quantiles$Q20, "alpha"=0.2, "colour"="goldenrod1")
      ),
      targetRange=list("lower"=0.6, "upper"=0.75),
    )
  expect_equal(class(rv), c("gg", "ggplot"))
})
