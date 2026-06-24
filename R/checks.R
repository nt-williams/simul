
check_list <- function(x) {
  if (!is.list(x)) {
    stop("`x` and `eif` should both be lists.", call. = FALSE)
  }
}

check_length <- function(x, y) {
  if (length(x) != length(y)) {
    stop("`x` and `eif` should have equal lengths.", call. = FALSE)
  }
}

check_nobs <- function(eif) {
  lengths <- vapply(eif, length, integer(1L))
  if (length(unique(lengths)) > 1L) {
    stop("All influence functions must have the same number of observations.", call. = FALSE)
  }
  if (lengths[1L] < 2L) {
    stop("Number of observations must be greater than 1.", call. = FALSE)
  }
}

check_eif_variance <- function(eif) {
  zero_var <- vapply(eif, function(x) var(x) == 0, logical(1))
  if (any(zero_var)) {
    stop("Variance of an influence function is zero; each influence function must have more than 1 observation.",
         call. = FALSE)
  }
}

check_level <- function(level) {
  if (!(level > 0 && level < 1)) {
    stop("Confidence level must be between 0 and 1.", call. = FALSE)
  }
}

multboot_checks <- function(x, eif, level) {
  check_list(x)
  check_list(eif)
  check_length(x, eif)
  check_nobs(eif)
  check_eif_variance(eif)
  check_level(level)
}
