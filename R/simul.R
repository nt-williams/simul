#' Find Simultaneous Confidence Band Critical Value
#'
#' @param x A list of parameter estimates, or a list of \code{ife::influence_func_estimate} objects.
#' @param eif A list of empirical efficient influence functions corresponding to the estimates in \code{x}.
#'   Not required when \code{x} is a list of \code{ife::influence_func_estimate} objects.
#' @param reps The number of repetitions to use for the multiplier bootstrap, the default is 1e5.
#' @param level The confidence level for the critical value should be calculated for, the default is 0.95.
#' @param ... Not used.
#'
#' @return The estimated critical value satisfying the requirements for a uniform confidence band around all estimates.
#' @export
#'
#' @examples
#' data(eifs)
#' psi <- lapply(eifs, function(x) x[[1]])
#' eif <- lapply(eifs, function(x) x[[2]])
#' simul(psi, eif, reps = 1000)
simul <- function(x, ...) UseMethod("simul")

#' @rdname simul
#' @export
simul.list <- function(x, eif = NULL, reps = 1e5, level = 0.95, ...) {
  is_ife <- vapply(x, inherits, logical(1L), "ife::influence_func_estimate")
  if (any(is_ife) && !all(is_ife)) {
    stop("All elements of `x` must be 'ife::influence_func_estimate' objects, or none of them.",
         call. = FALSE)
  }
  if (all(is_ife)) {
    eif <- lapply(x, function(o) o@eif)
    x   <- lapply(x, function(o) o@x)
  }
  multboot_checks(x, eif, level)
  mbs <- multBoot(eif, length(eif[[1]]), reps)
  return(quantile(mbs, level, names = FALSE))
}
