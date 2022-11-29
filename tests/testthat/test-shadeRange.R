test_that("shadeRange fails gracefully when given bad inputs", {
  expect_error(shadeRange(NULL))
  
  d <- tibble::tibble(x=c(0, 1), y=c(0, 1))
  p <- d %>% ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
  expect_error(p %>% shadeRange(NULL))
  expect_error(p %>% shadeRange(27))
  expect_error(p %>% shadeRange(list("lower"=0.25, "upper"=0.75, "colour"="blue")))
  expect_error(p %>% shadeRange(list("lower"=0.25, "upper"=0.75, "alpha"=0.3)))
  expect_error(p %>% shadeRange(list("lower"=0.25, "colour"="blue", "alpha"=0.3)))
  expect_error(p %>% shadeRange(list("upper"=0.25, "colour"="blue", "alpha"=0.3)))
})
