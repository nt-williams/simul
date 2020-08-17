
simul_crit <- function(x, eif, nobs, reps = 1e5, level = 0.95) {
  multboot_checks(x, eif, nobs, level)
  mbs <- UnifMultBoot(x, eif, nobs, reps)
  return(quantile(mbs, level, names = FALSE))
}
