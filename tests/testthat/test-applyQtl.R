test_that("applyQtl fails gracefull with bad inputs", {
  expect_error(applyQtl(NULL))
  expect_error(applyQtl(Sys.Date()))
  expect_error(applyQtl(tibble::tibble(good=1), bad))
  expect_error(applyQtl(tibble::tibble(good=1), good))
})

test_that("applyQtl works", {
  expect_equal(
    applyQtl(tibble::tibble(x=1:4), x, lower=2),
    tibble::tibble(x=1:2, Lower=2)
  )
  expect_equal(
    applyQtl(tibble::tibble(x=1:4), x, upper=3),
    tibble::tibble(x=3:4, Upper=3)
  )
  expect_equal(
    applyQtl(tibble::tibble(x=1:4), x, lower=1, upper=4),
    tibble::tibble(x=c(1, 4), Lower=1, Upper=4)
  )
})