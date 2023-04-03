# Optimizing the model to run faster
library(ggplot2) # For plotting

##### All parameters are as before.
# A. Model parameters: events
g <- 0.02     # "Germ size" of a transmitted strain [0.02]
m <- 20.0     # Growth constant of S. pneumoniae within host [20.0]
b <- 0.05     # Within-host growth "benefit" of sensitive strain [0.05]
beta <- 4.0   # Person-to-person transmission rate [4.0]
u <- 0.43     # Natural clearance rate [0.43]
tau <- 0.2    # Antibiotic treatment rate [0.2]

# B. Model parameters: granularity of simulation
dt <- 1/30   # Length of each time step in months [1/30]
years <- 1   # Number of years to simulate [1]
n <- 500     # Number of individuals to simulate [500]

##### C. We are no longer defining events in terms of functions operating upon a single
##### host, but handling this directly in the main simulation loop below. However, this
##### time, we are going to wrap all the code needed to run the simulation in its own
##### function. This will make it easier to run multiple simulations.
RunSimulation <- function()
{
    ##### Time steps to run simulation
    steps <- (12 * years)/dt

    ##### Create storage for all hosts -- as before.
    hosts <- data.frame(s = rep(0, n), r = rep(0, n))

    # Seed the simulation with some carriers of each strain
    hosts$s[1:10] <- 0.5
    hosts$r[1:10] <- 0.5

    ##### Create storage for simulation results -- as before.
    ##### This will be returned from the function at the end.
    results <- data.frame(time = (0:steps) * dt,
                     carriageS = 0, carriageR = 0,
                     host1s = 0, host1r = 0)

    # Store the initial conditions
    results$carriageS[1] <- sum(hosts$s)
    results$carriageR[1] <- sum(hosts$r)
    results$host1s[1] <- hosts$s[1]
    results$host1r[1] <- hosts$r[1]

    # Run the simulation
    bar <- txtProgressBar(min = 1, max = steps, style = 3)

    # Loop over each time step . . .
    for (ts in 1:steps)
    {
        # Calculate the total population carriage of each strain
        carriageS <- sum(hosts$s)
        carriageR <- sum(hosts$r)
    
        # Calculate force of infection of each strain
        lambdaS <- beta * carriageS / n
        lambdaR <- beta * carriageR / n
        
        # Save population state for this time step
        results$carriageS[ts + 1] <- sum(hosts$s)
        results$carriageR[ts + 1] <- sum(hosts$r)
        results$host1s[ts + 1] <- hosts$s[1]
        results$host1r[ts + 1] <- hosts$r[1]
    
        ##### This is where the changes come in.
        ##### Now, rather than looping through each host -- which is inefficient --
        ##### we will "vectorize" the dynamics so that each type of event is executed
        ##### with a few statements. This allows the code to run much more quickly.
        
        ##### This can be broken down into three steps for each event:
        ##### 1. Decide how many hosts experience the event this time step.
        ##### 2. Select which specific hosts will experience the event.
        ##### 3. Execute the event.
        ##### Then, after each event has been done:
        ##### 4. Run any additional logic that needs to be done for all hosts.
        
        ##### 1. To select which hosts experience a particular event, we first draw
        ##### from a binomial distribution to get the number N of hosts experiencing the
        ##### event (with n trials and success probability 1 - exp(-x * dt), where x is the
        ##### rate of the event happening). Then, we sample N integers from 1 to n, where
        ##### n is the total number of hosts, to select which hosts will experience the event.
        
        ##### To sample from a binomial distribution, use the rbinom function. We need to do
        ##### this for sensitive-strain transmission, resistant-strain transmission, clearance,
        ##### and treatment events.
        NExposeToS <- rbinom(1, n, 1 - exp(-lambdaS * dt))
        NExposeToR <- rbinom(1, n, 1 - exp(-lambdaR * dt))
        NClearance <- rbinom(1, n, 1 - exp(-u * dt))
        NTreatment <- rbinom(1, n, 1 - exp(-tau * dt))
    
        ##### 2. To select which hosts, we can use the sample function, with 1:n as the first 
        ##### parameter and the number of hosts affected by the event as the second parameter.
        iExposeToS <- sample(1:n, NExposeToS)
        iExposeToR <- sample(1:n, NExposeToR)
        iClearance <- sample(1:n, NClearance)
        iTreatment <- sample(1:n, NTreatment)
        
        ##### 3. To execute events, we use the same logic as the previous part of the practical,
        ##### but vectorise the events.
        hosts$s[iExposeToS] <- hosts$s[iExposeToS] + g
        hosts$r[iExposeToR] <- hosts$r[iExposeToR] + g
        hosts$s[iClearance] <- 0
        hosts$r[iClearance] <- 0
        hosts$s[iTreatment] <- 0
    
        ##### 4. Finally, for the logic that needs to happen to all hosts (i.e. within-host
        ##### growth and competition), we can operate on columns of the data frame all at once.
        ds_dt <- hosts$s * m * (1 + b - hosts$s - hosts$r)
        dr_dt <- hosts$r * m * (1 - hosts$s - hosts$r)
        hosts$s <- hosts$s + ds_dt * dt
        hosts$r <- hosts$r + dr_dt * dt
    
        # Update progress bar
        setTxtProgressBar(bar, ts)
        if (ts == steps) {
            close(bar)
        }
    }
    
    # Finally, return the results.
    return (results)
}

# Run the simulation
results <- RunSimulation()

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

##### G. Now that we have a faster-running version of the simulation, let's do
##### a little exploration of the impact of varying parameters on simulation
##### dynamics. One possibility is to increase n and years here to get less
##### noisy results, but feel free to adjust these as desired.
n <- 1000
years <- 10

##### We're going to do 20 runs, varying the treatment rate from tau_min to tau_max.
n_runs <- 20
tau_min <- 0
tau_max <- 0.4

##### Storage for results of this parameter sweep.
sweep <- data.frame(tau = rep(0, n_runs), carriageS = 0, carriageR = 0)

##### Do the parameter sweep
for (run in 1:n_runs)
{
    ##### Set tau and run the simulation.
    tau <- tau_min + (run - 1) * (tau_max - tau_min) / (n_runs - 1)
    results <- RunSimulation()
    
    #### Store results, with carriage averaged over the last 500 rows.
    rows <- nrow(results)
    sweep$tau[run] <- tau
    sweep$carriageS[run] <- mean(results$carriageS[(rows - 499):rows])
    sweep$carriageR[run] <- mean(results$carriageR[(rows - 499):rows])
}

##### Show results
ggplot(sweep) + 
    geom_line(aes(x = tau, y = carriageS), colour = "blue") + 
    geom_line(aes(x = tau, y = carriageR), colour = "red", linetype = "dashed") +
    labs(x = "Treatment rate (months^-1)", y = "Mean carriage density\nat end of simulation")
