library(ggplot2)     ## for plotting
library(adaptivetau) ## for stochastic simulations
library(data.table)  ## for manipulation of results

## In this practical session, we will use the
## "adaptivetau" package to run simulations.

# For adaptivetau, we first need to define the "transitions", or events that can
# happen in the model.
# In the SIR model, these are (from the Gillespie practical):
SIR_events

## We also need to specify a rate function. We have this already:
SIR_rates

# We also have initial values,
init.values

# parameters
parms

# and number of time steps
tmax

# These are all the components we need to use adaptive tau using the
# ssa.adaptivetau function
r <- ssa.adaptivetau(
  init.values, SIR_events, SIR_rates, parms, tf = tmax
)

nsim <- 100 ## number of trial simulations

# Again, we run multiple simulations and store them in a data frame, traj, which
# looks the same as the "lr" data frame from the last session, but containing
# multiple simulation runs and an additional column i that represents the
# column index
traj <- lapply(
  1:nsim, \(sample_id) data.table(ssa.adaptivetau(
    init.values, SIR_events, SIR_rates, parms, tf = tmax
  ))
) |> rbindlist(idcol = "sample_id")

#' @question Compare the run time of the command above to running 100
#' simulations using our Gillespie algorithm from Practical 1. How much is the
#' speed gain? You can use the "system.time" function for this.
#' @answer
















#' @question Re-write the code above to simulate from the SEIR model. Analyse
#' the outputs using the same routines as you did with the SIR model.
#' @answer
