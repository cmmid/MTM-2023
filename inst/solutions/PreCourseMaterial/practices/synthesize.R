
require(jsonlite)

# this construction allow interactive usage (run the first line)
# vs command line usage (the second line will overwrite the first)
# finally, to run this script from another script via source(...)
# can simply override commandArgs <- function(...) c(...whatever you want args to be...)
.args <- c("input/peru.json", "output/peru_synthetic_pop.csv")
.args <- commandArgs(trailingOnly = TRUE)

# parse arguments
file_parameters <- .args[1]
file_results    <- .args[2]

# read the parameters
list_parameters <- read_json(file_parameters)

# constants for population model
MAX_EXPOSURE_YEARS <- 30
NUM_STRAINS <- 4
NUM_SAMPLES <- 10000

# utility function for pre-allocating the population data structure
allocate_results <- function(num_samples) {
  return(data.frame(id = 1:num_samples, high_risk=NA, outcome=NA_integer_))
}

# function to simulate a population
# arguments:
#  num_samples, the size of the population
#  prob_high_risk, the probability of being high risk
#  prob_exposure_hr, the exposure probability for high risk individuals
#  prob_exposure_lr, the exposure probability for low risk individuals
synthesize_population <- function(num_samples, prob_high_risk, prob_exposure_hr, prob_exposure_lr) {
  # create a container for data
  population <- allocate_results(num_samples)
  
  # create a logical vector of which individuals will be high risk
  # TRUE == high risk
  which_high_risk <- runif(num_samples) < prob_high_risk
  population$high_risk <- which_high_risk
  
  # use that vector to get each individuals risk
  prob_risk <- ifelse(which_high_risk, prob_exposure_hr, prob_exposure_lr)
  
  # for each person in the population...
  for (id in 1:num_samples) {
    # get their number of exposures
    num_exposures <- rbinom(1, MAX_EXPOSURE_YEARS, prob_risk[id])

    # get the strain of those exposures
    exposure_strains <- sample(1:NUM_STRAINS, num_exposures, replace = TRUE)
    
    # since strains are immunizing, the number of infections == number of unique strains
    num_infections <- length(unique(exposure_strains))
    
    # store result
    population[id,]$outcome <- num_infections
  }
  return(population)
}

# synthesize a population using the input parameters
result <- synthesize_population(
  num_samples      = NUM_SAMPLES,
  prob_high_risk   = list_parameters$high_risk_proportion,
  prob_exposure_hr = list_parameters$high_risk_probability,
  prob_exposure_lr = list_parameters$low_risk_probability
);

# store the resulting population to the target file
write.csv(result, file_results, row.names = F)