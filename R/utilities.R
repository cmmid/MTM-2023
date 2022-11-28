
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

#' @title Reminders for Scripts
#'
#' @param custom, text to display after generic reminder
#'
#' @param text, the generic reminder
#'
#' @export
#' @examples
#' reminder("Some exhortation.")
reminder <- function(custom = NULL, text =
"For the script below, our general guidance is to run the code,
check the results in the console or plots tab, and then
answer the associated questions yourself or amongst a small group.

Answering some of the questions will entail changing function
arguments or filling in missing arguments. These are marked
with `TODO(...)` placeholders.

For some of those, we suggest relevant functions with
'@hint: ?FUNCTION', as in `?FUNCTION` will prompt you with the
documentation of a function that may be useful

There are some items marked with @aside; skip these until you
have completed all other elements of a particular activity."
) {
  message("\n", text)
  if (!is.null(custom)) message("\n", custom)
  invisible(NULL)
}

#' @title Patchwork Grid
#'
#' @description Provides a quick way to convert a named list structure
#' (possibly nested) of plots into a tagged grid arrangement of those plots.
#'
#' @param plist a list of named lists of [ggplot2] objects, representing a set
#' of plots to layout as a grid.
#'
#' @export
patchwork_grid <- function(plist) {
  # TODO more checks
  stopifnot(
    "`plist` not a list." = is.list(plist),
    "Empty `plist`." = length(plist) != 0
  )

  rown <- length(plist)
  coln <- length(plist[[1]])

  allps <- Reduce(c, plist)
  nms <- names(allps)

  return(
    Reduce(`+`, allps) +
    plot_annotation(tag_levels = list(nms)) +
    plot_layout(ncol = coln, nrow = rown) &
    theme(
      plot.tag.position = c(0.5, 1),
      plot.tag = element_text(vjust = 0)
    )
  )

}

#' @export
nested_lapply <- function(X, FUN, ...) {
  if (!is.null(names(X))) {
    return(lapply(X, FUN, ...))
  } else return(lapply(X, nested_lapply, FUN=FUN, ...))
}
