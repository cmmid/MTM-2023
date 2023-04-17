library('ggplot2') ## for plotting
library('adaptivetau') ## for stochastic simulations
library('data.table') ## for manipulation of results

## In this practical session, we will use the
## "adaptivetau" package to run simulations.

## For adaptivetau, we first need to define the "transitions", or events that can
## happen in the model. In the SIR model, these are:
transitions <- list(
  c(S = -1, I = +1),
  c(I = -1, R = +1))

## We then need to specify a rate function:
SIRrateF <- function(state, parms, time) {
  beta <- parms[["beta"]]
  gamma <- parms[["gamma"]]

  S <- state[["S"]]
  I <- state[["I"]]
  R <- state[["R"]]

  N <- S + I + R

  rates <- c(beta * S * I/N,
             gamma * I)

  return(rates)
}

## Next, we set some initial values
init.values <- c(S = 249, ## number of susceptibles
                 I = 10, ## number infectious
                 R = 0) ## number immune

## and parameters
parms <- c(beta = 2, ## infection rate
           gamma = 1) ## recovery rate

## Now, run a trial simulation for 60 time steps
tmax <- 10 ## number of time steps to simulate

r <- ssa.adaptivetau(init.values, transitions, SIRrateF, parms, tf=tmax)

nsim <- 100 ## number of trial simulations

## Again, we run multiple simulations and store them in a data frame, traj, which
## looks the same as the "lr" data frame from the last session, but containing
## multiple simulation runs and an additional column i that represents the
## column index
system.time(traj <- lapply(1:nsim, function(sample_id) ssa.adaptivetau(init.values, transitions, SIRrateF, parms, tf=tmax)) |>
              rbindlist(idcol = "sample_id"))

## EXERCISE: compare the run time of the command above to running 100
## simulations using our Gillespie algorithm from Practical 1. How much is the
## speed gain? You can use the "system.time" function for this.

## EXERCISE: re-write the code above to simulate from the SEITL model. Analyse
## the outputs using the same routines as you did with the SIR model.
