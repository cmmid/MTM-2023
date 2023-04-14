# from github.com/pearsonca/cabputils, (c) 2022 Carl A. B. Pearson
# reproduced here for their utility w/o having to add an additional dependency

#' @title [base::match.call] Extension
#'
#' @description Produces a `match.call` with defaults
#' also included.
#'
#' @inheritParams base::match.call
#'
match.call.defaults <- function(
  definition = sys.function(sys.parent()),
  call = sys.call(sys.parent()),
  expand.dots = TRUE,
  envir = parent.frame(2L)
) {
  # get the call
  mc <- match.call(definition, call, expand.dots, envir)
  # get the formals, tossing any ellipsis
  fs <- formals(definition, envir)
  fs$... <- NULL

  # for any arguments set in formals & not in the call
  for (nm in setdiff(names(fs), names(mc)))
    mc[[nm]] <- fs[[nm]] # add those to the call

  return(mc)
}

#' Generic Function Wrapper
#'
#' @description provides a convenience function for
#' producing duplicate functions with different
#' defaults
#'
#' @param FUN the function to wrap
#'
#' @param ... the new defaults
#'
#' @param .ENV the environment for the resulting
#' copy-function (i.e. where any variables will be
#' evaluated). NB, the default (`environment(FUN)`) is
#' mostly convenient, but can be dangerous e.g. by
#' replacing an important function
#'
#'
#' @return the new function
#'
rejig <- function(FUN, ..., .ENV = environment(FUN)) {
  # initial validation
  stopifnot(
    "FUN isn't a function." = is.function(FUN),
    "FUN is a primitive function." = !is.primitive(FUN)
  )

  dots <- as.list(match.call())[-1] # get some new defaults
  dots$FUN <- dots$.ENV <- NULL # drop all the not-defaults

  if (length(dots) == 0) {
    warning("... is empty. Just returning FUN.")
    return(FUN)
  }

  .FUN <- FUN # make a duplicate of FUN
  forms <- formals(FUN) # get the original defaults

  # potentially more validation: check for ... argument
  # in FUN and try to partial match all arguments in
  # rejig
  hasdots <- "..." %in% names(forms)
  replacements <- names(forms)[pmatch(names(dots), names(forms))]

  if (any(is.na(replacements)) && !hasdots) {
    errmsg <- sprintf("
FUN does not have ... argument, and
rejig ... arguments do not match FUN arguments:
%s
", names(dots)[is.na(replacements)] |> paste(collapse = ", ")
    )
    stop(errmsg)
  }

  # correct any partially matched defaults
  names(dots)[!is.na(replacements)] <- replacements[!is.na(replacements)]
  # set the new defaults
  formals(.FUN)[names(dots)] <- dots
# TODO: figure out the right way to do this
#  environment(.FUN) <- .ENV

  if (hasdots && any(is.na(replacements))) {
    # the internals of FUN may pass around the ellipsis, which now
    # excludes newly set default variables, so need to use it
    body(.FUN) <- substitute({
      mc <- MTM:::match.call.defaults()
      mc[[1]] <- FUN
      eval(mc)
    })
  }

  return(.FUN)

}
