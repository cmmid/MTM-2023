
# this construction allow interactive usage (run the first line)
# vs command line usage (the second line will overwrite the first)
# finally, to run this script from another script via source(...)
# can simply override commandArgs <- function(...) c(...whatever you want args to be...)
.args <- c("output/peru_synthetic_pop.csv", "output/chile_synthetic_pop.csv", "output/measurements.rds")
.args <- commandArgs(trailingOnly = TRUE)

# parse arguments
file_data_sources <- head(.args, -1)
file_results      <- tail(.args, 1)

# read in all the data...
df_all_data <- data.frame() # initially empty data.frame
for (src in file_data_sources) { # for each of the data files...
  tmp_df <- read.csv(src) # read in the data
  tmp_df$Country <- gsub("^output/([a-zA-Z]+)_.+$", "\\1", src) # annotate with the country
  df_all_data <- rbind(df_all_data, tmp_df) # update the final data set
}

# for use with aggregate; extras lo/median/hi quartiles in list format
calculate_quantiles <- function(x) {
  result <- quantile(x, probs = (1:3)/4)
  names(result) <- c("lo.q","med","hi.q")
  result
}

# this converts the results of multi-column (output) aggregate to pure data.frame
reformat_aggregate <- function(...) do.call(data.frame, aggregate(...))

# compute the quantiles of outcome (i.e., number of infections per individual)
# for all the data, organized by risk, Country, and the combination of the two
results <- list(
  by_country = reformat_aggregate(outcome ~ Country, df_all_data, calculate_quantiles),
  by_risk    = reformat_aggregate(outcome ~ high_risk, df_all_data, quantile),
  by_both    = reformat_aggregate(outcome ~ Country + high_risk, df_all_data, quantile)
)

# save the results as an R-data-structure
saveRDS(results, file_results)