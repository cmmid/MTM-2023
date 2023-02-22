# Embedding the within-host competition mode in a person-to-person transmission model
library(ggplot2) # For plotting

# A. Model parameters: events
g <- 0.02     # "Germ size" of a transmitted strain [0.02]
m <- 20.0     # Growth constant of S. pneumoniae within host [20.0]
b <- 0.05     # Within-host growth "benefit" of sensitive strain [0.05]

##### New event parameters
beta <- 4.0   # Person-to-person transmission rate [4.0]
u <- 0.43     # Natural clearance rate [0.43]
tau <- 0.2    # Antibiotic treatment rate [0.2]

# B. Model parameters: granularity of simulation
dt <- 1/30   # Length of each time step in months [1/30]
years <- 1   # Number of years to simulate [1]
steps <- (12 * years)/dt  # Time steps to run simulation

##### New granularity parameters
n <- 500     # Number of individuals to simulate [500]

# C. Events
# Here, we define, in functional form, all the events that can happen to a host.
# Each function takes a vector (s, r) of the host's sensitive and resistant strain carriage
# and evaluates to a vector of the same format.

# Exposure to sensitive strain
ExposeToS <- function(h) {
    list(s = h$s + g, r = h$r)
}

# Exposure to resistant strain
ExposeToR <- function(h) {
    list(s = h$s, r = h$r + g)
}

# Natural clearance by the immune system
Clearance <- function(h) {
    list(s = 0, r = 0)
}

# Antibiotic treatment 
Treatment <- function(h) {
    list(s = 0, r = h$r)
}

# Within-host growth and competition for one time step
Growth <- function(h) {
    ds_dt <- h$s * m * (1 + b - h$s - h$r)
    dr_dt <- h$r * m * (1 - h$s - h$r)
    list(s = h$s + ds_dt * dt, r = h$r + dr_dt * dt)
}

##### D. Event function -- for an event with rate x and time step dt, evaluates
##### to TRUE with the same probability that the event happens in a given time step.
Event <- function(x, dt) {
    return (runif(1) < ...)
}

##### E. Storage for all hosts -- this time we want a data frame rather than a list,
##### to track sensitive and resistant carriage for n individuals rather than just one
hosts <- data.frame(s = rep(0, n), ...)

# Seed the simulation with some carriers of each strain
hosts$s[1:10] <- 0.5
hosts$r[1:10] <- 0.5

##### Storage of simulation results
##### Note: in carriageS and carriageR, we will store the total amount of sensitive
##### and resistant strains in the population. In host1S and host1R, we will store
##### a time series of carriage for a single host -- just to verify that this is
##### all working the way we think it should.
results <- data.frame(time = (0:steps) * dt,
                      carriageS = 0, carriageR = 0,
                      host1s = 0, host1r = 0)

# Store the initial conditions
results$carriageS[1] <- sum(hosts$s)
results$carriageR[1] <- sum(hosts$r)
results$host1s[1] <- hosts$s[1]
results$host1r[1] <- hosts$r[1]


# E. The simulation itself.

# We'll use the built-in function txtProgressBar to track the simulation's progress.
# Really helps for planning coffee breaks! It needs to know the minimum and maximum
# values to expect, and style = 3 tells it to report the percentage complete.
bar <- txtProgressBar(min = 1, max = steps, style = 3)

# Loop over each time step . . .
for (ts in 1:steps)
{
    ##### Calculate the total population carriage of each strain
    ##### Hint: we can use the sum function for this
    carriageS <- ... 
    carriageR <- ...

    ##### Calculate force of infection of each strain
    ##### Hint: this is the population carriage of each strain, times the transmission rate
    lambdaS <- ...
    lambdaR <- ...

    ##### Save population state for this time step
    results$carriageS[ts + 1] <- ...
    ...

    # Loop through each host . . .
    for (i in 1:n)
    {
        # Transmission of S. pneumoniae sensitive strain: hosts become sensitive-strain 
        # carriers at rate lambdaS. We'll use our Event() function from above to run the
        # code with the correct probability.
        if (Event(lambdaS, dt)) {
            hosts[i,] <- ExposeToS(hosts[i,])
        }
        
        #### Fill in code for ExposeToR, Clearance, Treatment, and Growth
        ...
    }
    
    # Update progress bar with the current time step; close progress bar if we are finished
    setTxtProgressBar(bar, ts)
    if (ts == steps) {
        close(bar)
    }
}

# F. Plot simulation results
# 1. Whole population
ggplot(results) + 
    geom_line(aes(x = time, y = carriageS / n), colour = "blue") + 
    geom_line(aes(x = time, y = carriageR / n), colour = "red", linetype = "dashed") +
    labs(x = "Time (months)", y = "Mean carriage density")

# 2. Individual host
ggplot(results) +
    geom_ribbon(aes(x = time, ymin = 0, ymax = host1r), fill = "red") + 
    geom_ribbon(aes(x = time, ymin = host1r, ymax = host1r + host1s), fill = "blue") +
    labs(x = "Time (months)", y = "Carriage density")


##### QUESTIONS FOR DISCUSSION
##### 1. How does the prevalence of the resistant strain change depending on the treatment rate
##### and the sensitive strain's within-host growth benefit?
##### 2. Non-susceptibility to penicillin among circulating strains of S. pneumoniae was measured
##### at 81.4% among children under 5 in eastern Kenya in 2009-2010 (Kobayashi et al. 2017, BMC
##### Inf Dis 17:25). Can you approximate this resistance prevalence by adjusting model parameters?
