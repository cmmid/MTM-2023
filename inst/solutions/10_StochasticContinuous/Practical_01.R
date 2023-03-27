library('ggplot2') ## for plotting
library('data.table') ## for manipulation of results

#' @section Overview
#' In this practical session, we will simulate the SIR model using
#' *Gillespie's algorithm*. Take ~10 minutes for this section to review how the
#' algorithm works in practice.

#' Recall from the discussion: the Gillespie algorithm is about *events* and
#' *rates*. In the SIR model, there are two events:

SIR_events <- list(
  infection = c(S = -1, I = +1),
  recovery = c(I = -1, R = +1)
)

#' and the associated rates, in terms of a `state = list(S=..., I=..., R=...)`
#' and `parms = list(beta = ..., gamma = ...)` (and `time`, which is ignored
#' in the minimal SIR model):

SIR_rates <- function(time, state, parms) with(c(parms, as.list(state)), {
  N <- sum(state)
  return(c(infection = beta * S * I/N, recovery = gamma * I))
})

#' Then in terms of Gillespie's algorithm, a solver is
stochcont_solve <- function(init_state, transitions, rateFun, params, tf) {
  time <- 0 ## initialise time to 0
  results_df <- list(as.list(c(time=0, init_state)))
  x <- init_state
  while (time < tf) {
    ## update current rates
    rates <- rateFun(time, x, parms)
    if (sum(rates) > 0) { ## check if any event can happen
      ## time of next event
      time <- time + rexp(n=1, rate = sum(rates))
      ## check if next event is supposed to happen before end time
      if (time <= tf) {
        # sample the next event
        evt <- transitions[[sample(length(rates), 1, prob = rates)]]
        # use it to update x
        x[names(evt)] <- x[names(evt)] + evt
      } else { ## next event happens after end time
        time <- tf
      }
    } else { ## no event can happen - go straight to end time
      time <- tf
    }
    ## add new row to results data frame
    results_df[[length(results_df)+1]] <- list(time=time, x)
  }
  ## return results data frame
  return(rbindlist(results_df))
}

init.values <- c(S=249, I=1, R=0) ## initial state
parms <- list(beta=1, gamma=0.5) ## parameter vector
tmax <- 20 ## end time

r <- stochcont_solve(init.values, SIR_events, SIR_rates, parms, tmax)

## run Gillespie simulation
# r <- SIR_gillespie(init_state=init.values, parms=parms, tf=tmax)

## Now, plot the result (using ggplot or plot)

## first, convert the result into a long data frame for plotting
lr <- r |> melt.data.table(
  id.vars = "time", variable.name = "compartment", value.name = "count"
)

## plot the result
ggplot(lr, aes(x=time, y=count, colour=compartment)) +
  geom_line()

## re-run this a few times to convince yourself the output is different every time

## Next, run multiple simulation runs and plot a few of them
nsim <- 100 ## number of trial simulations

## We store the simulations in a data frame, traj, which
## contains the results from multiple simulation runs and an additional column
## that represents the simulation index
traj <- lapply(1:nsim, \(sample_id) SIR_gillespie(init.values, parms, tmax)) |>
  rbindlist(idcol = "sample_id")

## convert to long data frame
mlr <- traj |> melt.data.table(
  id.vars = c("sample_id", "time"), variable.name = "compartment", value.name = "count"
)

## Next, plot the multiple simulation runs
## (we only plot the I compartment)
ggplot(mlr,
       aes(x=time, y=count, group=sample_id, color=compartment)) +
  geom_line(alpha = 0.1) +
  facet_wrap(~compartment)

## You'll notice that some outbreaks die out very quickly, while some others are
## growing to affect large parts of the population. Let us plot the distribution of
## overall outbreak sizes. Which proportion of outbreaks dies out quickly?

## create data frame of outbreak sizes
outbreak_sizes <- mlr[
  compartment == "R",.(size = count[which.max(time)]), by=sample_id
]

## plot it as a histogram
ggplot(outbreak_sizes, aes(x=size)) +
  geom_histogram()

## determine number of large outbreaks (defined as larger than 50)
outbreak_sizes[size > 50, .N]

## Next, calculate the mean and standard deviation of each state across the
## multiple runs. To do that, we need the value of the different states at
## pre-defined time steps, whereas adaptivetau only returns the times at which
## certain events happened. In order to, for example, extract the values of the
## trajectory at integer time points, we can use
time.points <- seq(0, tmax, by=0.1)
timeTraj <-mlr[, .(
  time = time.points,
  count = approx(time, count, xout = time.points, method = "constant")$y
), by=.(sample_id, compartment)]

## Now, calculate a summary trajectory containing the mean and standard
## deviation (sd) of the number of infectious people at every time step
sumTraj <- timeTraj[compartment == "I",.(
  trajectories = "all", mean = mean(count), sd = sd(count)
), by = time]

## plot
ggplot(sumTraj, aes(x=time, y=mean, ymin=pmax(0, mean-sd), ymax=mean+sd)) +
  geom_line() +
  geom_ribbon(alpha=0.3)

## As a second summary, we only consider trajectories that have not gone
## extinct, that is where I>0

sumTrajGr0 <- timeTraj[
  (compartment=="I") & (count > 0),
  .(trajectories = "only >0", mean = mean(count), sd = sd(count)),
  by=time
]

iTraj <- rbind(sumTraj, sumTrajGr0)

## plot
ggplot(iTraj, aes(x=time, y=mean, ymin=pmax(0, mean-sd), ymax=mean+sd,
                  colour=trajectories, fill=trajectories)) +
  geom_line() +
  geom_ribbon(alpha=0.3) +
  scale_color_brewer(palette="Set1")

## We now compare the two averages to the deterministic trajectory

## Define model function (see practical 7)
library('deSolve')
SIR_model <- function(times, state, parms){
  ## Define variables
  S <- state["S"]
  I <- state["I"]
  R <- state["R"]
  N <- S + I + R
                                        # Extract parameters
  beta <- parms["beta"]
  gamma <- parms["gamma"]
                                        # Define differential equations
  dS <- - (beta * S * I) / N
  dI <- (beta * S * I) / N - gamma * I
  dR <- gamma * I
  res <- list(c(dS, dI, dR ))
  return(res)
}

ode_output_raw <-
  ode(y = init.values, times = seq(0, tmax), func = SIR_model, parms = parms,
      method = "rk4")
## Convert to data frame for easy extraction of columns
ode_output <- (as.data.table(ode_output_raw) |> melt.data.table(
  id.vars = "time", variable.name = "compartment", value.name = "mean"
))[compartment == "I", .(time, trajectories = "deterministic", mean, sd = 0)]

## Combine into one big data frame
allTraj <- rbind(
  ode_output,
  iTraj
)

## plot
ggplot(allTraj, aes(x=time, y=mean, colour=trajectories)) +
  geom_line() +
  scale_color_brewer(palette="Set1")

## Questions:
## 1. Compare the mean trajectory to some of the individual trajectories. In how
## much does it represent a "typical" trajectory?
## 2. Repeat the experiment with different values of the parameters. When does
## deterministic vs stochastic make more or less of a difference?
## 3. Rewrite the model code to be an SEIR model. How do the results differ?

