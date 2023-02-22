
# this main script runs all the sub-scripts with the appropriate arguments
commandArgs <- function(...) c("input/peru.json", "output/peru_synthetic_pop.csv")
source("synthesize.R")
commandArgs <- function(...) c("input/chile.json", "output/chile_synthetic_pop.csv")
source("synthesize.R")
commandArgs <- function(...) c("output/peru_synthetic_pop.csv", "output/chile_synthetic_pop.csv", "output/measurements.rds")
source("analyze.R")
commandArgs <- function(...) c(
  "output/peru_synthetic_pop.csv", "output/chile_synthetic_pop.csv",
  "output/measurements.rds",
  "figures/histogram_pop_comparison.png"
)
source("plot.R")