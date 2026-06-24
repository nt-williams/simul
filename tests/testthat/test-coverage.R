test_that("Correct coverage", {
  skip("Computationally intensive")

  simdata <- function() {
    x1 <- rnorm(100, 5)
    x2 <- rnorm(100, 2)
    x3 <- rnorm(100, 3)
    x4 <- rnorm(100, 10)

    list(
      ife::ife(mean(x1), x1 - mean(x1)), 
      ife::ife(mean(x2), x2 - mean(x2)),
      ife::ife(mean(x3), x3 - mean(x3)), 
      ife::ife(mean(x4), x4 - mean(x4))
    )
  }

  set.seed(3475)

  foo <- lapply(seq_len(1000), \(i) simdata())

  # foo1 <- do.call("rbind", lapply(foo, \(x) tidy(x[[1]])))
  # foo2 <- do.call("rbind", lapply(foo, \(x) tidy(x[[2]])))
  # foo3 <- do.call("rbind", lapply(foo, \(x) tidy(x[[3]])))
  # foo4 <- do.call("rbind", lapply(foo, \(x) tidy(x[[4]])))
  
  # mean((foo1$conf.low <= 5) & (foo1$conf.high >= 5))
  # mean((foo2$conf.low <= 2) & (foo2$conf.high >= 2))
  # mean((foo3$conf.low <= 3) & (foo3$conf.high >= 3))
  # mean((foo4$conf.low <= 10) & (foo4$conf.high >= 10))

  foo6 <- lapply(foo, \(x) simul(x, reps = 1000))

  for (i in seq_len(1000)) {
    for (j in seq_len(4)) {
      foo[[i]][[j]]@critical_value <- foo6[[i]]
    }
  }

  coverage <- lapply(foo, function(x) {
    tmp <- lapply(x, ife::tidy)
    ((tmp[[1]]$conf.low <= 5) & (tmp[[1]]$conf.high >= 5)) &
      ((tmp[[2]]$conf.low <= 2) & (tmp[[2]]$conf.high >= 2)) &
      ((tmp[[3]]$conf.low <= 3) & (tmp[[3]]$conf.high >= 3)) &
      ((tmp[[4]]$conf.low <= 10) & (tmp[[4]]$conf.high >= 10))
  }) |> 
    unlist() |> 
    mean()

  expect_equal(coverage, 0.95, tolerance = 0.01)
})
