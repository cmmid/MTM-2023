# SOLUTIONS: Individual-based SARS-CoV-2 transmission model, practical 2
library(ggplot2)


## Model parameters
beta <- 0.5        # Transmission parameter
iota <- 1e-5       # Importation rate
wane <- 0.05       # Rate of antibody waning

dt <- 1            # Time step of simulation (1 day)
days <- 365 * 2    # Duration of simulation (2 years)
steps <- days / dt # Total number of time steps
n <- 1000          # Population size


## Some helper functions
# Calculates infectiousness as a function of state and age: zero if state is
# not "I"; nonzero if state is "I", and slightly decreasing with age
infectiousness <- function(state, age) {
    ifelse(state == "I", 1.25 - age / 160, 0)
}

# Calculates susceptibility of individuals with antibody level(s) ab
susceptibility <- function(ab) {
    pnorm(ab, 5, 1)
}

# Generates n random delays from the latent-period distribution
# (approximately 2 days, on average)
latent_delay <- function(n) {
    rlnorm(n, meanlog = 0.5, sdlog = 0.6)
}

# Generates n random delays from the infectious-period distribution
# (approximately 5 days, on average)
infectious_delay <- function(n) {
    rlnorm(n, meanlog = 1.5, sdlog = 0.5)
}

# Generates n random increments to antibody levels following recovery
ab_increment <- function(n) {
    rnorm(n, mean = 12, sd = 2)
}


## Data frame to store simulation results
results <- data.frame(ts = 1:steps, S = 0, E = 0, I = 0, AMeanU = 0, AMeanV = 0)


## Initialize simulation

# Set the seed for the pseudorandom number generator, for reproducibility
set.seed(12345)

# Initialize state variables
state <- rep("S", n)   # Each individual's state: start with all susceptible
age <- runif(n, 0, 80) # Each individual's age: random distribution from 0 to 80
delay <- rep(0, n)     # Delay for latent and infectious periods
antib <- rep(0, n)     # Antibody concentration for each individual
vacc <- rep(FALSE, n)  # Vaccinated status

state[1:10] <- "E"     # Start 10 individuals in the "exposed" state


## Run simulation

# Initialize progress bar
bar <- txtProgressBar(min = 1, max = steps, style = 3)

# Loop over each time step . . .
for (ts in 1:steps) {
    # Calculate the force of infection
    lambda <- beta * sum(infectiousness(state, age)) / n + iota

    # Loop through each individual . . .
    for (i in 1:n) {
        # Update individual i's non-state variables
        # Time remaining in latent/infectious periods
        delay[i] <- delay[i] - dt
        # Antibody waning
        antib[i] <- antib[i] - wane * dt
        # Vaccination at time step 300 for over-40s
        if ((ts == 300) && (age[i] >= 40)) {
            vacc[i] <- TRUE
            antib[i] <- antib[i] + 2 * ab_increment(1)
        }

        # Update individual i's state
        if (state[i] == "S") {
            # Transition S -> E (infection) at rate lambda
            if (runif(1) < 1 - exp(-lambda * dt)) {
                if (runif(1) > susceptibility(antib[i])) {
                    state[i] <- "E"
                    delay[i] <- latent_delay(1)
                }
            }
        } else if (state[i] == "E") {
            # Transition E -> I (latent to infectious)
            if (delay[i] < 0) {
                state[i] <- "I"
                delay[i] <- infectious_delay(1)
            }
        } else if (state[i] == "I") {
            # Transition I -> S (infectious to susceptible)
            if (delay[i] < 0) {
                state[i] <- "S"
                antib[i] <- antib[i] + ab_increment(1)
            }
        }
    }

    # Save population state for this time step
    results[ts, "S"] <- sum(state == "S")
    results[ts, "E"] <- sum(state == "E")
    results[ts, "I"] <- sum(state == "I")
    results[ts, "AMeanU"] <- mean(antib[!vacc])
    results[ts, "AMeanV"] <- mean(antib[vacc])

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
  geom_line(aes(x = ts, y = I, colour = "I"))

ggplot(results) +
  geom_line(aes(x = ts, y = AMeanU, colour = "Unvaccinated")) +
  geom_line(aes(x = ts, y = AMeanV, colour = "Vaccinated")) +
  labs(x = "Time step", y = "Mean antibody level")
