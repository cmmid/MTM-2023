
#' The available tutorials
#'
#' @export
tutorial.list <- basename(list.dirs(system.file("tutorials", package = "MTM"), recursive = FALSE))

#' launches one of the Modern Techniques in Modelling (MTM) short course
#' sessions
#'
#' @param session, a string or integer, indicating which session to launch
#'
#'
#' @export
tutorial <- function(session) {
  if (is.integer(session)) session <- tutorial.list[session]
  session <- match.arg(session, MTM::tutorial.list)
  learnr::run_tutorial(session, package = "MTM")
}

#' Create a local copy of the MTM scripts.
#'
#' @param targetdir, path to enclosing directory. If this directory does not exist, will attempt to create it (recursively)
#' @param overwrite, overwrite existing files (corresponding to the course scripts)?
#' @param solutions, copy the `sol/` folder (which contains the solutions)
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
  targetdir = file.path("~", "Downloads", "MTM"),
  overwrite = FALSE,
  solutions  = FALSE
) {
  if(!all(!missing(targetdir), is.character(targetdir), length(targetdir) != 1)) {
    warning("mis-formed targetdir. Check that the argument is a string (single entry character vector)")
    return(NULL)
  }
  # TODO interactively check to allow creation of directory
  if (!dir.exists(targetdir[1]) && !dir.create(targetdir[1], recursive = T)) {
    warning(sprintf("'%s' does not exist and could not be created.", targetdir[1]))
    return(FALSE)
  }
  srcdir   <- system.file("scripts", package = "MTM")
  srcfiles <- list.files(
    srcdir, full.names = TRUE, recursive = FALSE, include.dirs = TRUE
  )

  res <- file.copy(from      = grep("sol", srcfiles, invert = TRUE, value = TRUE),
                   to        = targetdir,
                   overwrite = overwrite,
                   recursive = TRUE)

  if (solutions) {
    srcdir <- system.file(file.path("scripts", "sol"), package = "MTM")
    targetdir <- file.path(targetdir, "sol")
    dir.create(targetdir)
    res <- c(res, file.copy(from      = srcfiles,
                            to        = targetdir,
                            overwrite = overwrite,
                            recursive = TRUE))
  }


  if (!all(res)) {
    warning("Something may have gone wrong with copying.")
    return(NULL)
  } else return(targetdir)
}


scripts <- function(target.dir = file.path("~", "Downloads", "MTMpracticals")) {
  dir.create(target.dir)
  # copy stuff from inst into directory
}
