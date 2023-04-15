library(ggplot2)    # for plotting
library(data.table) # for manipulation of results
library(MTM)        # for definitions from earlier sessions
library(deSolve)    # for comparison to ODE model results

#' @section Overview
#'
#' In this practical session, we will simulate the SIR model using
#' *Gillespie's algorithm*. Take ~10 minutes for this section to review how the
#' algorithm works in practice.
#'
#' Recall from the discussion: the Gillespie algorithm is about *events* and
#' *rates*. In the SIR model, there are two events:

SIR_events <- list(
  infection = c(S = -1, I = +1), # i.e. infection => one S becomes an I
  recovery = c(I = -1, R = +1)   # i.e. recovery => one I becomes an R
)

#' and the associated rates, in terms of a `state = c(S=..., I=..., R=...)`
#' and `parms = list(beta = ..., gamma = ...)` (and `time`, which is ignored
#' in the minimal SIR model):

SIR_rates <- function(
  time, state, parms
) {
  with(c(parms, as.list(state)), {
    N <- S + I + R
    return(c(infection = beta * S * I / N, recovery = gamma * I))
  })
}

#' Then in terms of Gillespie's algorithm, a solver is:

stochcont_solve <- function(
  init.state, transitions = SIR_events,
  rateFun = SIR_rates, params, tf
) {
  time <- 0 ## initialise time to 0
  results_df <- list(as.list(c(time = 0, init.state)))
  x <- init.state
  while (time < tf) {
    ## update current rates e.g. c(infection = beta*S*I/N, recovery = gamma*I)
    rates <- rateFun(time, x, parms)
    if (sum(rates) > 0) { ## check if any event can happen
      ## time of next event
      time <- time + rexp(n = 1, rate = sum(rates))
      ## check if next event is supposed to happen before end time
      if (time <= tf) {
        # sample the next event - e.g. picks "infection" or "recovery"
        evt <- transitions[[sample(length(rates), 1, prob = rates)]]
        # use it to update x - e.g. apply S-1, I+1 or I-1, R+1
        x[names(evt)] <- x[names(evt)] + evt
      } else { ## next event happens after end time
        time <- tf
      }
    } else { ## no event can happen - go straight to end time
      time <- tf
    }
    ## add new row to results data frame
    results_df[[length(results_df) + 1]] <- as.list(c(time = time, x))
  }
  ## return results data frame
  return(rbindlist(results_df))
}

#' To use the solver, we apply it to some initial conditions, parameters
#' and the model-defining transitions and rates

init.values <- c(S = 249, I = 1, R = 0) ## initial state
parms <- list(beta = 1, gamma = 0.5) ## parameter vector
tmax <- 20 ## end time

r <- stochcont_solve(init.values, SIR_events, SIR_rates, parms, tmax)

#' @section Visualisation
#'
#' @question Now, plot the result (using [ggplot2::ggplot()] or [plot()]).
#' @answer

# convert the result into long format
lr <- r |> melt.data.table(
  id.vars = "time", variable.name = "compartment", value.name = "count"
)
lr

# plot the result
ggplot(lr, aes(x = time, y = count, color = compartment)) +
  geom_line()

#' @question Repeat the above steps (run => plot) a few times - are the results
#' different every time?
#' @answer Yes, though it may be hard to tell for some.
#'
#' @section Sampling
#'
#' Let's run many simulations, gather them, and plot them to get a better idea
#' of how different trajectories can be. We're going to run the simulation
#' `nsim` times, then bind up all the results, with a column `sample_id`
#' indicating which simulation results go together.

nsim <- 100 ## number of trial simulations

traj <- lapply( # for all of ...
  1:nsim,       # samples 1 -> nsim, run the Gillespie solver function
  \(sample_id) stochcont_solve(init.values, SIR_events, SIR_rates, parms, tmax)
) |> rbindlist(idcol = "sample_id")
# ...and the bind the results, id'd by their sample

## convert to long data frame
mlr <- traj |> melt.data.table(
  id.vars = c("sample_id", "time"), variable.name = "compartment",
  value.name = "count"
)
mlr

#' @question Now visualise all of the trajectories for the *I*nfectious
#' compartment on a single plot.
#' @answer

ggplot(mlr) + aes(
  x = time, y = count, group = sample_id, color = compartment
) +
  geom_line(alpha = 0.25) +
  facet_wrap(~compartment) +
  scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1))) +
  theme_minimal() + theme(
    legend.position = c(0.5, 0.95), legend.justification = c(0.5, 1)
  )

#' Some outbreaks die out very quickly, while some others are
#' growing to affect large parts of the population.
#'
#' @question Plot the distribution of overall outbreak sizes.
#' Which proportion of outbreaks dies out quickly?
#'
#' @answer

## create data frame of outbreak sizes
outbreak_sizes <- mlr[
  compartment == "R", .(size = count[which.max(time)]), by = sample_id
]

## plot it as a histogram
ggplot(outbreak_sizes, aes(x = size)) +
  geom_histogram() + theme_minimal()

## determine number of large outbreaks (defined as larger than 50)
outbreak_sizes[size > 50, .N]

#' @section Outcome Distribution
#'
#' Next, let's calculate the mean and standard deviation of each state across
#' the multiple runs. However, because Gillespie algorithm results may happen at
#' different times, we need to do some work first. Specifically, we need the
#' value of the different states at pre-defined times. In order to, for example,
#' extract the values of the trajectory at 1/10th time points, we can use:

time.points <- seq(0, tmax, by = 0.1)
timeTraj <- mlr[, .(
  time = time.points,
  count = approx(time, count, xout = time.points, method = "constant")$y
), by = .(sample_id, compartment)]

#' Now, we can calculate a summary trajectory mean and standard
#' deviation (sd) for the *I*nfectious people at our preferred time steps:

sumTraj <- timeTraj[compartment == "I", .(
  compartment, trajectories = "all", mean = mean(count), sd = sd(count)
), by = time]

#' Let's compare these summaries to the original trajectories, for the *I*
#' compartment:
ggplot() + aes(x = time, color = compartment, fill = compartment) +
  geom_ribbon(
    aes(ymin = pmax(0, mean - sd), ymax = mean + sd, color = NULL),
    data = sumTraj, alpha = 0.2
  ) +
  geom_line(
    aes(y = count, group = sample_id), data = mlr[compartment == "I"],
    alpha = 0.25
  ) +
  geom_line(aes(y = mean, group = NULL), data = sumTraj) +
  theme_minimal() + theme(
    legend.position = c(0.05, 0.95), legend.justification = c(0, 1)
  )

#' @question Does it seem like the mean +/- sd envelope represents the
#' sample trajectories lines?
#' @answer Just considering the outbreaks that actually happen, not really!

#' Now let's look at second summary, where we only consider trajectories that
#' have not gone extinct, i.e. where `I > 0`

sumTrajGr0 <- timeTraj[
  (compartment == "I") & (count > 0),
  .(compartment, trajectories = "only >0", mean = mean(count), sd = sd(count)),
  by = time
]

#' and considering the same kind of comparison:

ggplot() + aes(x = time, color = compartment, fill = compartment) +
  geom_ribbon(
    aes(ymin = pmax(0, mean - sd), ymax = mean + sd, color = NULL),
    data = sumTrajGr0, alpha = 0.2
  ) +
  geom_line(
    aes(y = count, group = sample_id),
    data = mlr[compartment == "I"], alpha = 0.25
  ) +
  geom_line(aes(y = mean, group = NULL), data = sumTrajGr0) +
  theme_minimal() + theme(
    legend.position = c(0.05, 0.95), legend.justification = c(0, 1)
  )

#' Let's look more closely at the difference between the with and without
#' extinct trajectories:

iTraj <- rbind(sumTraj, sumTrajGr0)
ggplot(iTraj) +
  aes(x = time, y = mean, ymin = pmax(0, mean - sd), ymax = mean + sd) +
  geom_line(aes(colour = trajectories)) +
  geom_ribbon(aes(fill = trajectories), alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() + theme(
    legend.position = c(0.05, 0.95), legend.justification = c(0, 1)
  )

#' Lastly, let's compare these results to the deterministic SIR model from an
#' earlier session. Recall the ODE SIR model with beta & gamma parameterization:

sampling_dSIR_betagamma

#' We can get the time series of this using [deSolve::ode()]:

ode_output_raw <- ode(
  y = init.values, times = time.points,
  func = sampling_dSIR_betagamma,
  parms = parms,
  method = "rk4"
)

## Convert to data frame for easy extraction of columns
ode_output <- (as.data.table(ode_output_raw) |> melt.data.table(
  id.vars = "time", variable.name = "compartment", value.name = "mean"
))[compartment == "I", .(
  time, compartment, trajectories = "deterministic", mean, sd = 0
)]

## Combine into one big data frame
allTraj <- rbind(
  ode_output,
  iTraj
)

## plot
ggplot(allTraj, aes(x = time, y = mean, colour = trajectories)) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() + theme(
    legend.position = c(0.05, 0.95), legend.justification = c(0, 1)
  )

#' @question Compare the mean trajectory to some of the individual trajectories:
#' In how much does it represent a "typical" trajectory?
#'
#' @question Repeat the experiment with different parameter values. When does
#' deterministic vs stochastic make more or less of a difference?
#'
#' @question Rewrite the code to be an SEIR model. How do the results differ?
