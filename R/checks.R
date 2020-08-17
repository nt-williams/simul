
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

check_nobs <- function(nobs, eif) {
  nobs_eif <- lapply(eif, function(x) length(x))
  if (!all(nobs_eif == nobs)) {
    stop("Mismatch between specified number of observations and length of efficient influence function(s).",
         call. = FALSE)
  }
}

check_level <- function(level) {
  if (!(level > 0 && level < 1)) {
    stop("Confidence level must be between 0 and 1.", call. = FALSE)
  }
}

multboot_checks <- function(x, eif, nobs, level) {
  check_list(x)
  check_list(eif)
  check_nobs(nobs, eif)
  check_length(x, eif)
  check_level(level)
}
