# Individual-based SARS-CoV-2 transmission model, practical 1
library(ggplot2)


## Model parameters
beta <- 0.5        # Transmission parameter
delta <- 1 / 2.5   # Rate of transitioning out of latent state
gamma <- 1 / 5     # Rate of transitioning out of infectious state
omega <- 1 / 180   # Rate of waning immunity

dt <- 1            # Time step of simulation (1 day)
days <- 365        # Duration of simulation (365 days)
steps <- days / dt # Total number of time steps
n <- 1000          # Population size


## Data frame to store simulation results
results <- data.frame(ts = 1:steps, S = 0, E = 0, I = 0, R = 0)


## Initialize simulation

# Set the seed for the pseudorandom number generator, for reproducibility
set.seed(12345)

# Since this is an individual-based model, we track the properties of all n
# individuals in the simulation. One kind of property we can track is a state,
# such as S (susceptible), E (exposed), I (infectious), or R (recovered). We
# will store each individual's state as a string, either "S", "E", "I", or "R".

state <- rep("S", n)   # Each individual's state: start with all susceptible
state[1:10] <- "E"     # Start 10 individuals in the "exposed" state


## Run simulation

# We'll use the built-in function txtProgressBar to track the simulation's
# progress. Really helps for planning coffee breaks! It needs to know the
# minimum and maximum values to expect, and style = 3 tells it to report the
# percentage complete.
bar <- txtProgressBar(min = 1, max = steps, style = 3)

# Loop over each time step . . .
for (ts in 1:steps) {
    # Calculate the force of infection
    lambda <- beta * sum(state == "I") / n

    # Loop through each individual . . .
    for (i in 1:n) {
        if (state[i] == "S") {
            # Transition S -> E (infection) at rate lambda
            if (runif(1) < 1 - exp(-lambda * dt)) {
                state[i] <- "E"
            }
        } else if (state[i] == "E") {
            # Transition E -> I (latent to infectious) at rate delta
            if (runif(1) < 1 - exp(-delta * dt)) {
                state[i] <- "I"
            }
        } else if (state[i] == "I") {
            # Transition I -> R (infectious to recovered) at rate gamma
            if (runif(1) < 1 - exp(-gamma * dt)) {
                state[i] <- "R"
            }
        } else if (state[i] == "R") {
            # Transition R -> S (waning of immunity) at rate omega
            if (runif(1) < 1 - exp(-omega * dt)) {
                state[i] <- "S"
            }
        }
    }

    # Save population state for this time step
    results[ts, "S"] <- sum(state == "S")
    results[ts, "E"] <- sum(state == "E")
    results[ts, "I"] <- sum(state == "I")
    results[ts, "R"] <- sum(state == "R")

    # Update progress bar; close progress bar if we are finished
    setTxtProgressBar(bar, ts)
    if (ts == steps) {
        close(bar)
    }
}

## Plot simulation results
ggplot(results) +
  geom_line(aes(x = ts, y = S, colour = "S")) +
  geom_line(aes(x = ts, y = E, colour = "E")) +
  geom_line(aes(x = ts, y = I, colour = "I")) +
  geom_line(aes(x = ts, y = R, colour = "R"))
