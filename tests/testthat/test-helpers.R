test_that(".columnExists fails gracefully", {
  expect_error(.columnExists(NULL, NULL))
  expect_error(1:2 %>% .columnExists(NULL))
})

test_that(".columnExists works", {
  expect_true(mtcars %>%  .columnExists(mpg))
  expect_false(mtcars %>%  .columnExists(bad))
})

test_that(".assertColumnExists works", {
  expect_error(mtcars %>%  .assertColumnExists(mpg), NA)
  expect_error(mtcars %>%  .assertColumnExists(bad))
})