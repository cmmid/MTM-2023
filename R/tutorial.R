
tutorial.list <- basename(list.dirs(system.file("tutorials", package = "MTM"), recursive = FALSE))

#' @export
tutorial <- function(session) {
  session <- match.arg(session, tutorial.list)
  learnr::run_tutorial(session, "MTM")
}