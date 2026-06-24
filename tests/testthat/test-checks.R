test_that("x and eif must be lists", {
  expect_error(multboot_checks(c(1, 2, 3), list(1, 2, 3), 0.95),
               "`x` and `eif` should both be lists.")
  expect_error(multboot_checks(list(1, 2, 3), c(1, 2, 3), 0.95),
               "`x` and `eif` should both be lists.")
})

test_that("x and eif must have the same length", {
  eif3 <- list(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3))
  expect_error(multboot_checks(list(1, 2, 3, 4), eif3, 0.95),
               "`x` and `eif` should have equal lengths.")
})

test_that("all eif elements must have the same number of observations", {
  expect_error(
    multboot_checks(list(1, 2), list(c(1, 2), c(3, 4, 5)), 0.95),
    "All influence functions must have the same number of observations."
  )
})

test_that("nobs must be greater than 1", {
  expect_error(multboot_checks(list(1), list(c(1)), 0.95),
               "Number of observations must be greater than 1.")
})

test_that("eif elements must have non-zero variance", {
  expect_error(
    multboot_checks(list(1, 2), list(rep(1, 5), c(1, 2, 3, 4, 5)), 0.95),
    "Variance of an influence function is zero"
  )
})

test_that("confidence level must be between 0 and 1", {
  eif2 <- list(c(1, 2), c(3, 4), c(5, 6))
  expect_error(multboot_checks(list(1, 2, 3), eif2, 0),
               "Confidence level must be between 0 and 1.")
  expect_error(multboot_checks(list(1, 2, 3), eif2, 1),
               "Confidence level must be between 0 and 1.")
  expect_error(multboot_checks(list(1, 2, 3), eif2, 95),
               "Confidence level must be between 0 and 1.")
})
