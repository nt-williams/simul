data(eifs)
psi <- lapply(eifs, function(x) x[[1]])
eif <- lapply(eifs, function(x) x[[2]])

test_that("simul.list returns a positive scalar", {
  set.seed(1)
  cv <- simul(psi, eif, reps = 500)
  expect_true(is.numeric(cv))
  expect_length(cv, 1)
  expect_gt(cv, 0)
})

test_that("simul.list result is sensitive to level", {
  set.seed(1); cv_95 <- simul(psi, eif, reps = 2000, level = 0.95)
  set.seed(1); cv_80 <- simul(psi, eif, reps = 2000, level = 0.80)
  expect_gt(cv_95, cv_80)
})

test_that("simul.list is reproducible with set.seed", {
  set.seed(42); cv1 <- simul(psi, eif, reps = 500)
  set.seed(42); cv2 <- simul(psi, eif, reps = 500)
  expect_identical(cv1, cv2)
})

test_that("simul is an S3 generic with a list method", {
  expect_true(isS3stdGeneric(simul))
  expect_false(is.null(getS3method("simul", "list", optional = TRUE)))
})

test_that("simul.list detects ife objects and agrees with explicit call", {
  skip_if_not_installed("ife")
  ife_objs <- lapply(seq_along(eif), function(i) ife::ife(x = psi[[i]], eif = eif[[i]]))

  set.seed(1); cv_list <- simul(psi, eif, reps = 500)
  set.seed(1); cv_ife  <- simul(ife_objs, reps = 500)
  expect_identical(cv_list, cv_ife)
})

test_that("simul.list errors on mixed ife and non-ife elements", {
  skip_if_not_installed("ife")
  o <- ife::ife(x = psi[[1]], eif = eif[[1]])
  expect_error(simul(list(o, psi[[2]]), reps = 500),
               "All elements of `x` must be 'ife::influence_func_estimate' objects, or none of them.")
})

test_that("simul.list handles a single ife object", {
  skip_if_not_installed("ife")
  o <- ife::ife(x = psi[[1]], eif = eif[[1]])
  set.seed(1)
  cv <- simul(list(o), reps = 500)
  expect_true(is.numeric(cv))
  expect_length(cv, 1)
  expect_gt(cv, 0)
})
