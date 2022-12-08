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

test_that(".assertColumnDoesNotExist works", {
  expect_error(tibble::tibble(good=1) %>%  .assertColumnDoesNotExist(bad), NA)
  expect_error(tibble::tibble(good=1) %>%  .assertColumnDoesNotExist(good))
})

test_that(".ensureLimitsAreNamed fails gracefully with bad inputs", {
  expect_error(.ensureLimitsAreNamed(1:3, nameStrings=c("a", "b")))
})

test_that(".ensureLimitsAreNamed works", {
  expect_true(is.null(.ensureLimitsAreNamed(NULL)))
  # Unnamed scalars and vectors
  expect_equal(names(.ensureLimitsAreNamed(0.2)), "action")
  expect_equal(names(.ensureLimitsAreNamed(c(0.2))), "action")
  expect_equal(names(.ensureLimitsAreNamed(1:2)), c("action", "warn"))
  expect_equal(names(.ensureLimitsAreNamed(1:2, decreasing = TRUE)), c("action", "warn"))
  expect_equal(names(.ensureLimitsAreNamed(1:3)), c("1", "2", "3"))
  # Named vector
  expect_equal(names(.ensureLimitsAreNamed(c("a"=1, "b"=2))), c("a", "b"))
  # Custom names
  expect_equal(names(.ensureLimitsAreNamed(1:2, nameStrings=c("a", "b"))), c("a", "b"))
  expect_equal(names(.ensureLimitsAreNamed(1:2, nameStrings=c("a", "b", "c"))), c("a", "b"))
})

test_that(".addStatusToObservedData fails gracefully with bad inputs", {
  expect_error(.addStatusToObservedData(NULL))
  expect_error(.addStatusToObservedData(tibble::tibble(good=1), bad))
})
