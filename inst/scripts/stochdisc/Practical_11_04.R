# Comparing models with and without co-colonisation
library(ggplot2) # For plotting

# A. Model parameters: events
g <- 0.02     # "Germ size" of a transmitted strain [0.02]
m <- 20.0     # Growth constant of S. pneumoniae within host [20.0]
b <- 0.05     # Within-host growth "benefit" of sensitive strain [0.05]
beta <- 4.0   # Person-to-person transmission rate [4.0]
u <- 0.43     # Natural clearance rate [0.43]
tau <- 0.2    # Antibiotic treatment rate [0.2]

##### New parameter
k <- 1.0      # Relative rate of co-colonisation [1.0]

# B. Model parameters: granularity of simulation
dt <- 1/30   # Length of each time step in months [1/30]
years <- 10  # Number of years to simulate [1]
n <- 1000    # Number of individuals to simulate [500]

##### See the lines marked with ### for differences from before.
RunSimulation <- function()
{
    # Time steps to run simulation
    steps <- (12 * years)/dt

    # Create storage for all hosts -- as before.
    hosts <- data.frame(s = rep(0, n), r = rep(0, n))

    # Seed the simulation with some carriers of each strain
    hosts$s[1:10] <- 0.5
    hosts$r[1:10] <- 0.5

    # Create storage for simulation results -- as before.
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

        ##### 1. Select number of events of each type
        ##### We now treat colonisation and co-colonisation as events with separate rates
        nCarriers <- sum(hosts$s + hosts$r > 0) #####
        nNonCarriers <- n - nCarriers #####
        NColoniseS <- rbinom(1, nNonCarriers, 1 - exp(-lambdaS * dt)) #####
        NColoniseR <- rbinom(1, nNonCarriers, 1 - exp(-lambdaR * dt)) #####
        NCoColoniseS <- rbinom(1, nCarriers, 1 - exp(-k * lambdaS * dt)) #####
        NCoColoniseR <- rbinom(1, nCarriers, 1 - exp(-k * lambdaR * dt)) #####
        NClearance <- rbinom(1, n, 1 - exp(-u * dt))
        NTreatment <- rbinom(1, n, 1 - exp(-tau * dt))
    
        ##### 2. Choose hosts
        iColoniseS <- sample(which(hosts$s + hosts$r == 0), NColoniseS) #####
        iColoniseR <- sample(which(hosts$s + hosts$r == 0), NColoniseR) #####
        iCoColoniseS <- sample(which(hosts$s + hosts$r > 0), NCoColoniseS) #####
        iCoColoniseR <- sample(which(hosts$s + hosts$r > 0), NCoColoniseR) #####
        iClearance <- sample(1:n, NClearance)
        iTreatment <- sample(1:n, NTreatment)
        
        # 3. Execute events
        hosts$s[iColoniseS] <- hosts$s[iColoniseS] + g #####
        hosts$r[iColoniseR] <- hosts$r[iColoniseR] + g #####
        hosts$s[iCoColoniseS] <- hosts$s[iCoColoniseS] + g #####
        hosts$r[iCoColoniseR] <- hosts$r[iCoColoniseR] + g #####
        hosts$s[iClearance] <- 0
        hosts$r[iClearance] <- 0
        hosts$s[iTreatment] <- 0
    
        # Within-host growth and competition
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
k <- 0
results <- RunSimulation()

##### C. Plot simulation results
##### What is different about the results of this trial simulation?
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

##### D. Parameter sweep with and without co-colonisation
n <- 1000
years <- 10

# 10 runs each, varying the treatment rate from tau_min to tau_max.
n_runs <- 10
tau_min <- 0
tau_max <- 0.4

# Storage for results of this parameter sweep.
sweep <- data.frame(tau = rep(0, n_runs * 3), k = 0, carriageS = 0, carriageR = 0)

# Do the parameter sweep
j <- 1
for (run in 1:n_runs)
{
    # Show overall progress
    print(paste(run, "/", n_runs))
    
    # Run with no, intermediate, and full co-colonisation
    for (k1 in c(0, 0.5, 1.0))
    {
        # Set tau and k and run the simulation.
        tau <- tau_min + (run - 1) * (tau_max - tau_min) / (n_runs - 1)
        k <- k1
        results <- RunSimulation()

        # Store results, with carriage averaged over the last 500 rows.
        rows <- nrow(results)
    
        sweep$tau[j] <- tau
        sweep$k[j] <- k
        sweep$carriageS[j] <- mean(results$carriageS[(rows - 499):rows])
        sweep$carriageR[j] <- mean(results$carriageR[(rows - 499):rows])
        j <- j + 1
    }
}

# Show results
ggplot(sweep) + 
    geom_line(aes(x = tau, y = carriageS), colour = "blue") + 
    geom_line(aes(x = tau, y = carriageR), colour = "red", linetype = "dashed") +
    facet_grid(k~.) +
    labs(x = "Treatment rate (months^-1)", y = "Mean carriage density\nat end of simulation")
