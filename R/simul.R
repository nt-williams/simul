
#' Find Simultaneous Confidence Band Critical Value
#'
#' @param x A list of parameter estimates.
#' @param eif A list of empirical efficient influence functions corresponding to the estimates in \code{x}.
#' @param nobs The number of observations.
#' @param reps The number of repetitions to use for the multiplier bootstrap, the default is 1e5.
#' @param level The confidence level for the critical value should be calculated for, the default is 0.95.
#'
#' @return The estimated critical value satisfying the requirements for a uniform confidence band around all estimates.
#' @export
#'
#' @examples
#' # TODO
simul_crit <- function(x, eif, nobs, reps = 1e5, level = 0.95) {
  multboot_checks(x, eif, nobs, level)
  mbs <- UnifMultBoot(x, eif, nobs, reps)
  return(quantile(mbs, level, names = FALSE))
}
