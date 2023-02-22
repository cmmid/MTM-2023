
require(ggplot2)

# this construction allow interactive usage (run the first line)
# vs command line usage (the second line will overwrite the first)
# finally, to run this script from another script via source(...)
# can simply override commandArgs <- function(...) c(...whatever you want args to be...)
.args <- c("output/peru_synthetic_pop.csv", "output/chile_synthetic_pop.csv", "output/measurements.rds", "figures/histogram_pop_comparison.png")
.args <- commandArgs(trailingOnly = TRUE)

# parse arguments
file_population_sims <- grep("\\.csv$", .args, value=T)
file_analysis_results <- grep("\\.rds$", .args, value=T)
file_results      <- tail(.args, 1)

# read in all the population data...
df_all_data <- data.frame() # initially empty data.frame
for (src in file_population_sims) { # for each of the data files...
  tmp_df <- read.csv(src) # read in the data
  tmp_df$Country <- gsub("^output/([a-zA-Z]+)_.+$", "\\1", src) # annotate with the country
  df_all_data <- rbind(df_all_data, tmp_df) # update the final data set
}

# read in the analysis results
df_analysis <- readRDS(file_analysis_results)

ucfirst <- function(str) paste0(toupper(substr(str, 1, 1)), substr(str, 2, nchar(str)))

plot_result <- ggplot(df_all_data) +
  aes(x=outcome, fill=Country, alpha=high_risk) +
  facet_grid(Country ~ ., labeller = labeller(Country=ucfirst)) +
  geom_histogram(position = "stack", bins = 5) +
  geom_vline(aes(xintercept=outcome.lo.q-.1, linetype="Lower Quartile", color=Country), df_analysis$by_country) +
  geom_vline(aes(xintercept=outcome.med, linetype="Median", color=Country), df_analysis$by_country) +
  geom_vline(aes(xintercept=outcome.hi.q+.1, linetype="Upper Quartile", color=Country), df_analysis$by_country) +
  scale_alpha_manual(
    "Risk Group",
    labels = function(high_risk) c("Low","High")[high_risk+1],
    values = c(`FALSE`=0.5,`TRUE`=0.7)
  ) +
  scale_x_continuous("Number of Infections") +
  scale_fill_discrete(labels=ucfirst) +
  scale_color_discrete(labels=ucfirst) +
  scale_linetype_manual("Measure", values=c(`Lower Quartile`="dashed",Median="solid",`Upper Quartile`="dotted")) +
  theme_minimal() + theme(panel.grid.major.x = element_blank())

# save the results as an R-data-structure
ggsave(file_results, plot_result)