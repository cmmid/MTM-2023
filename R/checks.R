
check_natural <- function(N) {
  stopifnot(
    "'N' must be an integer." = is.integer(N) || all(N == as.integer(N)),
    "'N' must be positive." = N > 0
  )
  invisible(N)
}

check_probability <- function(p) {
  stopifnot(
    "'p' must be numeric." = is.numeric(p),
    "'p' must be within [0,1]" = all((0 <= p) & (p <= 1))
  )
  invisible(p)
}

check_scalar <- function(x) {
  stopifnot("'x' must be a scalar." = length(x) == 1)
  invisible(x)
}