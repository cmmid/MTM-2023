#'
#' The `checks.R` script is intended to include `check_...` functions
#' used within various session functions. They are not intended to be
#' exported (though can be seen with `MTM:::check_...`). They are
#' generally meant to be ignored by students, and used to provide
#' informative errors when the functions that *are* exported are
#' misapplied.
#'
#' Exported functions, meant to be used directly by students in
#' practicals, should have their arguments verified by one or
#' more `check_...` functions.
#'
#' In general, `check_...`s should:
#'  - concern one feature at a time
#'  - invisibly return the checked value
#'  - be applied via pipe
#'

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
  stopifnot("'x' must be a scalar." = length(x) == 1L)
  invisible(x)
}
