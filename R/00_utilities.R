
#' @title Provides a placeholder that students can fill in
#'
#' @param message, a string; explains what students should do at some point in
#' scripts / tutorials
#'
#' @export
TODO <- function(message) stop(message, call. = FALSE)

#' @title function to pretty-print help text to the console
#'
#' @export
tutorial_help <- function(h) {
  if (!is.character(h)) h <- as.character(expression(h))
  tools:::Rd2txt(utils:::.getHelpFile(h))
}

#' @title defines reference colors used in plots throughout MTM course
#'
#' @export
SIRcolors <- c(S = "dodgerblue", I = "firebrick", R = "forestgreen")

#' @title a `learnr::question_text` wrapper
#'
#' @param text, see `learnr::question_text`
#' @param reply, a string, allowing *bold* and _italic_ markdown indicators,
#' which provides an example answer
#'
#' @details `learnr::quiz` framework doesn't support free-response style
#' Q&A. This function provides a way for participants to provide an open-end
#' response, which isn't explicitly checked, and to receive an example
#' reply after submitting their answer
#'
#' @export
question_freeresponse <- function(text, reply, ...) question_text(
  text,
  answer("", correct = TRUE),
  incorrect = NULL,
  message = reply,
  ...
)

#' `gg_scale_wrapper` enables easy creation of re-usable `ggplot` scales
#' (a Facade Factory, if familiar with Design Patterns)
#'
#' @param scale_fun the `ggplot` scale function that will ultimately be called
#' @param ... whatever default arguments for that scale function; the
#'   ones you define will override the defaults
#'
#' @return the `scale_fun`, except with defaults set
#'
#' @examples
#'
#' require(ggplot)
#' require(MTM)
#' scale_color_SIR <- gg_scale_wrapper(scale_color_manual, values = SIRcolors)
#'
#' @export
gg_scale_wrapper <- function(
    scale_fun,
    ...
) {
  stopifnot(!missing(scale_fun))
  defs <- list(...)
  if (!length(defs)) warning(
    "provided no default arguments; consider using scale_fun directly."
  )

  return(function(...) {
    # this different ... is how you get a function back that let's you
    # override defaults, set other arguments to scale_... functions
    .ellipsis <- list(...)
    .args <- defs
    .args[names(.ellipsis)] <- .ellipsis
    do.call(scale_fun, .args)
  })
}

#' `gg_theme_wrapper` enables easy creation of re-usable `ggplot` sem-themes
#' (a Facade Factory, if familiar with Design Patterns)
#'
#' @param ... whatever default arguments for the `theme` function; the
#'   ones you define will override the defaults
#'
#' @return a `theme` function, except with defaults set
#'
#' @export
gg_theme_wrapper <- function(
    ...
) {
  defs <- list(...)
  if (!length(defs)) warning(
    "provided no default arguments; consider using theme directly."
  )

  return(function(...) {
    # this different ... is how you get a function back that let's you
    # override defaults, set other arguments to `theme` function
    .ellipsis <- list(...)
    .args <- defs
    .args[names(.ellipsis)] <- .ellipsis
    do.call(ggplot2::theme, .args)
  })
}

#' `gg_geom_wrapper` enables easy creation of re-usable `ggplot` geoms
#' (a Facade Factory, if familiar with Design Patterns)
#'
#' @param geom_fun the `ggplot` geom function that will ultimately be called
#' @param ... whatever default arguments for that geom; the
#'   ones you define will override the defaults
#'
#' @return the `geom_fun`, except with defaults set
#'
#' @examples
#'
#' require(ggplot)
#' scale_color_SIR <- gg_scale_wrapper(scale_color_manual, values = SIRcolors)
#'
#' @export
gg_geom_wrapper <- function(
    geom_fun,
    ...
) {
  stopifnot(!missing(geom_fun))
  defs <- list(...)
  if (!length(defs)) warning(
    "provided no default arguments; consider using geom_fun directly."
  )

  return(function(...) {
    # this different ... is how you get a function back that let's you
    # override defaults, set other arguments to geom_... functions
    .ellipsis <- list(...)
    .args <- defs
    .args[names(.ellipsis)] <- .ellipsis
    do.call(geom_fun, .args)
  })
}
