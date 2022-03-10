require(igraph)
require(ggplot2)
# BELOW IS THE REFERENCE CODE; IF YOU WANT PRACTICALS TO RUN, DON'T MODIFY IT
# Warnings aside: you should examine the code here, as you may find the
# constructs helpful, and the practical scripts apply these functions
# Note: this code is commented using roxygen2 style

#' Pre-allocates an appropriately sized container for Reed-Frost model results
#' 
#' @param n, an integer; the number of individuals in the model
#' @details The maximum number of steps in a Reed-Frost model corresponds to one
#' new infection (no more, no less) each time step + a final time step with that
#' last infectious individual recovering.
#' 
#' @return a n+1 x 3 matrix of `NA_integer_`s with rows labeled by timestep, columns labeled by state.
#'
rf_prealloc <- function(n) matrix(
  NA_integer_,
  nrow = n+1, ncol = 3,
  dimnames = list(
    timestep=0:n,
    state=c("S","I","R")
  )
)

#' Trims the pre-allocated container to only the relevant results
#' 
#' @param result, a ? x 3 matrix, originally constructed w/ \link{rf_prealloc} and
#' subsequently populated with simulation results
#' @return a ? x 3 matrix, trimmed to only the timesteps that matter; final row corresponds to
#' when the outbreak is over
rf_trim <- function(result) result[!is.na(result[,"I"]),]

#' A simple Reed-Frost Model Implementation
#'
#' @return a matrix; each row is the number of S, I, and R at a particular time step
#'         first row is always ${ I=1, S=n-1, R=0 }$
#' @example 
#' cbs <- chainbinom_sim(n=30, p=0.05) # simulate for a population of 30, transmission probabilty = 5%
#' 
chainbinom_sim <- function(n, p, i) {
  set.seed(i)
  state = c(S=n-1,I=1,R=0)
  result <- rf_prealloc(n)
  tm <- 1
  while(state["I"]) {
    result[tm,] <- state # record the state
    pinf <- 1 - (1-p)^state["I"] # determine the probability of infection
    infs <- rbinom(1, state["S"], pinf) # determine the number of new infections
    state["R"] <- state["R"] + state["I"] # update R (all I -> R)
    state["S"] <- state["S"] - infs # update S (infected S -> I)
    state["I"] <- infs # update I (all I -> R, infected S -> I)
    tm <- tm+1 # increment time
  }
  result[tm,] <- state # record the final state
  return(rf_trim(result))
}

# this takes care of sampling w/ a simulator,
# by resetting random number generator, extracting
# results, etc
#' Convenience harness for replicating function runs
#' 
#' @param num_samples, an integer vector; if length > 1, used as sample ids + run seeds. Otherwise
#'   sample ids are 1:num_samples
#' @param simfunc, a function representing the simulation; it will be called with the contents of ...
#' @param observef, a function that measures the simulation, e.g. getting final size
#' @param ..., any arguments to simfunc
#' 
#' @return a matrix, num_samples x 1 + number of columns returned by observef
#' 
sampler <- function(num_samples, simfunc, observef, ...) {
  smp_ids <- if (length(num_samples)==1) 1:num_samples else num_samples
  return(data.frame(t(sapply(smp_ids, function(i) {
    res <- simfunc(..., i=i);
    c(sample=i, observef(res))
  }))))
}

rf_observer <- function(res) c(duration = dim(res)[2]-1, final_size = tail(res,1)[3])


#' plotter
#' 
#' Run, then plot the results of, two different Reed-Frost simulators
#'
#' @param simulator_A one simulator, a function that can receive whatever supplied in ...
#' @param simulator_B another simulator, a function that can receive whatever supplied in ...
#' @param samples, how many replicates for each simulator
#' @param labels, the simulator names, for plotting
#' @return a ggplot object; also prints the plot to current device as a side effect
plotter <- function(simulator_A, simulator_B = chainbinom_sim, samples=100, labels=c(A="Network", B="Chain-Binomial"), ...) {
  resA <- sampler(num_samples=samples, simfunc=simulator_A, observef=rf_observer, ...)
  resB <- sampler(num_samples=samples, simfunc=simulator_B, observef=rf_observer, ...)
  resA$implementation <- labels["A"]
  resB$implementation <- labels["B"]
  plot_data <- rbind(resA, resB)
  return(
    ggplot(plot_data) + aes(x=final_size) +
      facet_grid(implementation ~ .) +
      geom_histogram(binwidth = 1) + theme_minimal()
  )
}

refgraphs <- list.files(path="../graphs", pattern = "*.csv", full.names = T)
