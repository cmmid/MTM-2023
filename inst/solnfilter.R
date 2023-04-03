
.args <- if (interactive()) {
  file.path("inst/scripts/solutions/network/00_Warmup.R")
} else commandArgs(trailingOnly = TRUE)

srcfile <- .args[1]
tarfile <- gsub("solutions/","",srcfile)

srclines <- readLines(srcfile)
tarlines <- srclines

answering <- FALSE
for (i in seq_along(srclines)) {
  if (answering) {
    # still answering?
    answering <- !startsWith(srclines[i], "#' @")
    if(answering) {
      tarlines[i] <- if (startsWith(srclines[i], "#'")) "#' " else ""
    }
  } else {
    # start answering?
    answering <- startsWith(srclines[i], "#' @answer")
    if(answering) {
      tarlines[i] <- "#' @answer"
    }
  }
}

writeLines(tarlines, tarfile)
