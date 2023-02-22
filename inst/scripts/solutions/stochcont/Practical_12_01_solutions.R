library('ggplot2') ## for plotting
library('tidyverse') ## for manipulation of results

## In this practical session, we will simulate the SIR model using the
## Gillespie algorithm. The first bit of code below is a function that simulates
## an SIR model using the Gillespie algorithm. Take 10 minutes to familiarise
## yourself with the function and make sure you understand its inner workings.
## Then, run it using the commands below.

## Function SIR_gillespie.
## This takes three arguments:
## - init_state: the initial state
##   (a named vector containing the number in S, I and R)
## - parms: the parameters
##   (a named vector containing the rates beta and gamma)
## - tf: the end time
SIR_gillespie <- function(init_state, parms, tf) {

  time <- 0 ## initialise time to 0

  ## assign parameters to easy-access variables
  beta <- parms["beta"]
  gamma <- parms["gamma"]

  ## assign states to easy-access variables
  S <- init_state["S"]
  I <- init_state["I"]
  R <- init_state["R"]
  N <- S + I + R

  ## create results data frame
  results_df <- data.frame(time=0, t(init_state))

  ## loop until end time is reached
  while (time < tf) {
    ## update current rates
    rates <- c()
    rates["infection"] <- beta * S * I / N
    rates["recovery"] <- gamma * I

    if (sum(rates) > 0) { ## check if any event can happen
      ## time of next event
      time <- time + rexp(n=1, rate=sum(rates))
      ## check if next event is supposed to happen before end time
      if (time <= tf) {
        ## generate cumulative sum of rates, to determine the type of the next
        ## event
        cumulative_rates <- cumsum(rates)
        ## determine type of next event
        type <- runif(n=1, min=0, max=sum(rates))
        if (type < cumulative_rates["infection"]) {
          ## infection
          S <- S - 1
          I <- I + 1
        } else if (type < cumulative_rates["recovery"]){
          ## recovery
          I <- I - 1
          R <- R + 1
        }
      } else { ## next event happens after end time
        time <- tf
      }
    } else { ## no event can happen - go straight to end time
      time <- tf
    }
    ## add new row to results data frame
    results_df <- rbind(results_df, c(time=time, S=S, I=I, R=R))
  }
  ## return results data frame
  return(results_df)
}

init.values <- c(S=249, I=1, R=0) ## initial state
parms <- c(beta=1, gamma=0.5) ## parameter vector
tmax <- 20 ## end time

## run Gillespie simulation
r <- SIR_gillespie(init_state=init.values, parms=parms, tf=tmax)

## Now, plot the result (using ggplot or plot)

## first, convert the result into a long data frame for plotting
lr <- as.data.frame(r) %>%
  gather(variable, value, 2:ncol(.))

## plot the result
ggplot(lr, aes(x=time, y=value, colour=variable)) +
  geom_line()

## re-run this a few times to convince yourself the output is different every time

## Next, run multiple simulation runs and plot a few of them
nsim <- 100 ## number of trial simulations

## We store the simulations in a data frame, traj, which
## contains the results from multiple simulation runs and an additional column
## that represents the simulation index
traj <- tibble(i=1:nsim) %>%
  rowwise() %>%
  mutate(trajectory=list(as.data.frame(
           SIR_gillespie(init.values, parms, tmax)))) %>%
  unnest(trajectory)

## convert to long data frame
mlr <- traj %>%
  gather(compartment, value, 3:ncol(.))

## Next, plot the multiple simulation runs
## (we only plot the I compartment)
ggplot(mlr,
       aes(x=time, y=value, group=i, color=compartment)) +
  geom_line() +
  facet_wrap(~compartment)

## You'll notice that some outbreaks die out very quickly, while some others are
## growing to affect large parts of the population. Let us plot the distribution of
## overall outbreak sizes. Which proportion of outbreaks dies out quickly?

## create data frame of outbreak sizes
outbreak_sizes <- mlr %>%
  filter(compartment=="R") %>%
  group_by(i) %>%
  filter(time==max(time)) %>%
  select(i, size=value)

## plot it as a histogram
ggplot(outbreak_sizes, aes(x=size)) +
  geom_histogram()

## determine number of large outbreaks (defined as larger than 50)
outbreak_sizes %>%
  filter(size > 50) %>%
  nrow

## Next, calculate the mean and standard deviation of each state across the
## multiple runs. To do that, we need the value of the different states at
## pre-defined time steps, whereas adaptivetau only returns the times at which
## certain events happened. In order to, for example, extract the values of the
## trajectory at integer time points, we can use
timeTraj <- mlr %>%
  group_by(i, compartment) %>%
  summarise(traj=list(data.frame(
              time=seq(0, tmax, by=0.1),
              value=approx(x=time, y=value, xout=seq(0, tmax, by=0.1),
                           method="constant")$y))) %>%
  unnest(traj)

## Now, calculate a summary trajectory containing the mean and standard
## deviation (sd) of the number of infectious people at every time step
sumTraj <- timeTraj %>%
  filter(compartment=="I") %>%
  group_by(time) %>%
  summarise(mean=mean(value),
            sd=sd(value))

## plot
ggplot(sumTraj, aes(x=time, y=mean, ymin=pmax(0, mean-sd), ymax=mean+sd)) +
  geom_line() +
  geom_ribbon(alpha=0.3)

## As a second summary, we only consider trajectories that have not gone
## extinct, that is where I>0
sumTrajAll <- sumTraj %>%
  mutate(trajectories="all")

sumTrajGr0 <- timeTraj %>%
  filter(compartment=="I", value > 0) %>%
  group_by(time) %>%
  summarise(mean=mean(value),
            sd=sd(value)) %>%
  mutate(trajectories="greater_than_zero")

iTraj <- bind_rows(sumTrajAll, sumTrajGr0)

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
ode_output <- as.data.frame(ode_output_raw)

## Combine into one big data frame
allTraj <- ode_output %>%
  gather(compartment, value, 2:ncol(.)) %>% ## convert to long format
  filter(compartment=="I") %>%
  rename(mean=value) %>% ## in deterministic, mean=value
  mutate(trajectories="deterministic", ## label trajectories
         sd=0) %>% ## in deterministic, sd=0
  bind_rows(iTraj)

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

