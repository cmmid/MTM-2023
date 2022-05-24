
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
