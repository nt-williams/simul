test_that("detect errors", {
  expect_error(multboot_checks(c(1, 2, 3), list(1, 2, 3), 1))
  expect_error(multboot_checks(list(1, 2, 3, 4), list(1, 2, 3), 1))
  expect_error(multboot_checks(list(1, 2, 3), list(1, 2, 3), 2))
  expect_error(multboot_checks(list(1, 2, 3), list(1, 2, 3), 1, 95))
})
