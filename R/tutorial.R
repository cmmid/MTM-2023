
#' The available tutorials
#'
tutorial.list <- basename(
  list.dirs(system.file("tutorials", package = "MTM"), recursive = FALSE)
)

#' launches one of the Modern Techniques in Modelling (MTM) short course
#' sessions
#'
#' @param session, a string or integer, indicating which session to launch
#'
launch <- function(session) {
  if (requireNamespace("learnr", quietly = TRUE)) {
    if (is.integer(session)) session <- tutorial.list[session]
    session <- match.arg(session, MTM::tutorial.list)
    learnr::run_tutorial(session, package = "MTM")
  } else {
    warning("Launching the tutorials requires 'learnr' package installed.")
  }
}

checked_dir_create <- function(path, ...) {
  r <- readline(prompt = sprintf(
    "Try to create path '%s' ? (y/anything else == no) ", path
  ))
  if ((r != "") && switch(r,
    Y = , y = TRUE,
    FALSE
  )) {
    dir.create(path, ...)
  } else {
    invisible(FALSE)
  }
}

check_overwrite <- function(path) {
  return(readline(prompt = paste0(
    "Are you sure you want to overwrite files in '", path, "'? (y/n) "
  )) != "y")
}

#' Create a local copy of the MTM scripts.
#'
#' @param path, string; the path to enclosing directory. If this directory does
#'   not exist, will create it
#'
#' @param overwrite, logical; overwrite existing files (corresponding to the
#'   course scripts)?
#'
#' @param what, string; the "scripts" or the "solutions"?
#'
#' @details This function creates a local copy of the R scripts associated with
#' the Modern Techniques in Modelling (MTM) short course, for students to edit
#' and run during learning activities. These scripts should only require the
# `MTM` package and associated dependencies.
#'
#' The created directory contains several subdirectories, each corresponding to
#' a different session. Within each directory, the exercise files are generally
#' named `NN_short_description.R` (where NN is 00, 01, etc - corresponding to
#' the expected order of completion, with the 00 session being background /
#' preparatory content).
#'
#' If used with default `what` argument, you will get a "scripts" subdirectory,
#' and if you ask for `what = "solutions"` there will be a "solutions"
# subdirectory. Both will contain sessions by name, scripts by order and
# shortname, without and with solutions filled in, respectively.
#'
#' @return string or `NULL`; non-`NULL` indicates successful creation + copy and
#' is the top level root of the course material.
#'
#' @examples
#' require(MTM)
#' tardir <- scripts()
#' list.files(tardir, recursive = TRUE)
#'
#' @export
scripts <- function(
  path = file.path(
    if (.Platform$OS.type == "windows") {
      Sys.getenv("USERPROFILE")
    } else { "~" },
    "Downloads", "MTM"
  ),
  overwrite = FALSE,
  what = c("scripts", "solutions")
) {
  stopifnot(
    "'path' must be a string." = is.character(path),
    "'path' must be a single string." = length(path) == 1
  )

  refpath <- match.arg(what, several.ok = FALSE)
  path <- file.path(path, refpath)

  dir_exists <- dir.exists(path)
  if (!dir_exists && !checked_dir_create(path, recursive = TRUE)) {
    warning(sprintf("'%s' does not exist and/or was not created.", path))
    return(invisible())
  }

  # Confirm user-requested overwrite
  if (
    overwrite && dir_exists && check_overwrite(path)
  ) {
    return(path)
  }

  srcdir   <- system.file(refpath, package = "MTM")
  srcfiles <- list.files(
    srcdir, full.names = TRUE, recursive = FALSE, include.dirs = TRUE
  )

  res <- file.copy(from      = srcfiles,
                   to        = path,
                   overwrite = overwrite,
                   recursive = TRUE)

  # `file.copy` returns `FALSE` if `overwrite == FALSE` and the directory
  # exists, and we don't want that to trigger a warning; hence the
  # `(overwrite || !dir_exists)` here
  if ((overwrite || !dir_exists) && !all(res)) {
    warning("Something may have gone wrong with copying.")
    return(invisible())
  } else {
    return(path)
  }
}

updateto <- function(target, what, paths) {
  stopifnot(is.character(paths)) # TODO other checks on path?
  rt <- "https://raw.githubusercontent.com/cmmid/MTM/master/inst/"
  updates <- paths |>
    lapply(\(item) paste0(rt, what, item) |> url() |> readLines())
  ps <- paste0(target, what, paths)
  for (i in seq_along(ps)) {
    writeLines(updates[[i]], ps[i])
  }
}

#' @title Update MTM Package
#'
#' @description
#' Updates the `MTM` package in place, or optionally just specific scripts or
#' solutions
#'
#' @param scripts Optional, particular scripts to update
#'
#' @param solutions Optional, particular solutions to update; defaults to
#' `scripts` - i.e., if requesting updated script, will also update
#' corresponding solution
#'
#' @param path Optional, but required if either `scripts` and/or `solutions` is
#' non-`NULL`; the (root) path to the scripts or solutions to update. Should
#' match whatever [scripts()] `path` used initially (and does so by default).
#'
#' @export
pkg_update <- function(
  path = file.path(
    if (.Platform$OS.type == "windows") {
      Sys.getenv("USERPROFILE")
    } else { "~" },
    "Downloads", "MTM"
  ), scripts = NULL, solutions = scripts
) {
  # the default case is to attempt to update the package
  if (is.null(scripts) & is.null(solutions)) {
    data.table::update_dev_pkg(
      object = "MTM", repo = "https://cmmid.github.io/MTM"
    )
  } else { # we're going to just fetch particular scripts
    if (!is.null(scripts)) {
      updateto(path, "scripts", scripts)
    }
    if (!is.null(solutions)) {
      updateto(path, "solutions", scripts)
    }
  }
}
