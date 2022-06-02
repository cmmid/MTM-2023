
#' The available tutorials
#'
#' @export
tutorial.list <- basename(list.dirs(system.file("tutorials", package = "MTM"), recursive = FALSE))

#' launches one of the Modern Techniques in Modelling (MTM) short course
#' sessions
#'
#' @param session, a string or integer, indicating which session to launch
#'
#' @export
launch <- function(session) {
  if (requireNamespace("learnr", quietly = TRUE)) {
    if (is.integer(session)) session <- tutorial.list[session]
    session <- match.arg(session, MTM::tutorial.list)
    learnr::run_tutorial(session, package = "MTM")
  } else {
    warning("Launching the tutorials requires 'learnr' package installed.")
  }
}

checked.dir.create <- function(path, ...) {
  stopifnot(is.character(path), length(path) == 1)
  r <- readline(prompt = sprintf("Try to create path '%s' ? (y/anything else == no) ", path))
  if((r != "") && switch(r,
    Y = , y = TRUE,
    FALSE
  )) { dir.create(path, ...) } else invisible(FALSE)
}

#' Create a local copy of the MTM scripts.
#'
#' @param path, string; the path to enclosing directory. If this directory does not exist, will create it
#' @param overwrite, boolean; overwrite existing files (corresponding to the course scripts)?
#' @param solutions, boolean; copy the `sol/` folder (which contains the solutions)
#'
#' @details This function creates a local copy of the R scripts associated with
#' the Modern Techniques in Modelling (MTM) short course, for students to edit
#' and run during learning activities. These scripts should only require the `MTM`
#' package and associated dependencies.
#'
#' The created directory contains several subdirectories, each corresponding to
#' a different session. Within each directory, the exercise files are generally named
#' `NN_short_description.R` (where NN is 00, 01, etc - corresponding to the expected
#' order of completion, with the 00 session being background / preparatory content).
#'
#' If used with `solutions = TRUE` argument, there will also be a `sol/` subdirectory,
#' which will then contain again sessions by name, scripts by order and shortname, with
#' solutions filled in.
#'
#' @return string or NULL; non-NULL indicates successful creation + copy and is the
#' top level root of the course material.
#'
#' @examples
#' require(MTM)
#' tardir <- scripts()
#' list.files(tardir, recursive = TRUE)
#'
#' @export
scripts <- function(
  path = file.path("~", "Downloads", "MTM"),
  overwrite = dir.exists(path),
  solutions  = FALSE
) {
  stopifnot(
    "'path' must be a string." = is.character(path),
    "'path' must be a single string." = length(path) == 1
  )
  if (!dir.exists(path) && !checked.dir.create(path, recursive = T)) {
    warning(sprintf("'%s' does not exist and/or was not be created.", path))
    return(NULL)
  }

  # TODO interactively check overwrite?

  srcdir   <- system.file("scripts", package = "MTM")
  srcfiles <- list.files(
    srcdir, full.names = TRUE, recursive = FALSE, include.dirs = TRUE
  )

  res <- file.copy(from      = grep("sol", srcfiles, invert = TRUE, value = TRUE),
                   to        = path,
                   overwrite = overwrite,
                   recursive = TRUE)

  if (solutions) {
    res <- c(res, file.copy(from      = grep("sol", srcfiles, value = TRUE),
                            to        = path,
                            overwrite = overwrite,
                            recursive = TRUE))
  }


  if (!all(res)) {
    warning("Something may have gone wrong with copying.")
    return(NULL)
  } else return(path)
}
