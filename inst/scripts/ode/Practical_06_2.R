######################################################
#              ODE's in R: Practical 2                 #
######################################################

# Load in the deSolve package
library(deSolve)
# If the package is not installed, install using the install.packages() function

########## 1 - Time dependent transmission rate

# The code below is for an SI model with a time dependent transmission rate.
# The transmission rate is a function of the maximum value of beta
# and the period of the cycle in days

# Define model function 
SI_seasonal_model <- function(times, state, parms){
  ## Define variables
  S <- state["S"]
  I <- state["I"]
  N <- S + I
  # Extract parameters
  beta_max <- parms["beta_max"]
  period <- parms["period"]
  # Calculate time dependent transmission rate
  beta <- beta_max / 2 * (1 + sin(times * (2 * pi / period) ) )
  
  # Define differential equations
  dS <- - (beta * S * I) / N
  dI <- (beta * S * I) / N
  res <- list(c(dS, dI))
  return(res)
}

# Define parameter values
parameters <- c(beta_max = 0.4, period = 10)

# Define time to solve equations
times <- seq(from = 0, to = 50, by = 1)

#a) Plot the equation of beta against a vector of times to understand the time
# dependent pattern. 
# How many days does it take to complete a full cycle? 
# How does this relate to the period?

#Answer:

##### YOUR CODE GOES HERE #####


# b) Now solve the model using the function ode and plot the output 
# HINT: you need to write the state vector and use the ode function
# using func = SI_seasonal_model

##### YOUR CODE GOES HERE #####


# c) Change the period to 50 days and plot the output. What does the model output look like?
# Can you explain why?

# Answer: 


########## 2 - Using evnts in deSolve

## deSolve can also be used to include 'events'. 'events' are triggered by 
# some specified change in the system. 

# For example, assume an SI model with births represents infetcion in a livestock popuation.
# If more than half of the target herd size becomes infected, the infected animals are culled
# at a daily rate of 0.5. 

# As we are using an open population model, we have two additional parameters.
# The birth rate, b, and the target herd size (or carrying capacity), K

# Define model function 
SI_open_model <- function(times, state, parms){
  ## Define variables
  S <- state["S"]
  I <- state["I"]
  N <- S + I
  # Extract parameters
  beta <- parms["beta"]
  K <- parms["K"]
  b <- parms["b"]
  # Define differential equations
  dS <- b * N * (K - N) / K - ( beta * S * I) / N
  dI <- (beta * S * I) / N 
  res <- list(c(dS, dI))
  return(res)
}

# Define time to solve equations
times <- seq(from = 0, to = 100, by = 1)

# Define initial conditions
N <- 100
I_0 <- 1
S_0 <- N - I_0
state <- c( S = S_0, I = I_0)

# a) Using beta = 0 (no infection risk), K = 100, b = 0.1, and an entirely susceptible population
# (I_0 = 0) investigate how the population grows with S_0 = 1, 50 and 100

# What size does the population grow to? Why is this?
# Answer: 

# How do you increase this threshold?
# Answer:

# Define parameter values
# K is our target herd size,b is the birth rate)
parameters <- c(beta = 0, K = 100, b = 0.1)

# To include an event we need to specify two functions 1) the root function and 
# 2) the event function 

# 1) The root function is used to trigger the event 
root <- function(times, state, parms){
  ## Define variables
  S <- state["S"]
  I <- state["I"]
  N <- S + I
  # Extract parameters
  K <- parms["K"]
  
  # Our condition : more than half of the target herd size becomes infected
  condition <- I <  K * 0.5 # This is a logical condition (TRUE/FALSE)
  return(as.numeric(condition)) # Make this numeric, event occours if root == 0
}

# 2) The event function describes what happens if the event is triggered
event_I_cull <- function(times, state, parms) {
  ## Define variables
  I <- state["I"]

  # Extract parameters
  tau <- parms["tau"]
  
  I <- I * (1 - tau) # Cull the infected population
  
  state["I"] <- I # Record new value of I
  
  return(state)
}

# We add the culling rate tau to our parameter vector 
parameters <- c(beta = 0.1, K = 100, tau = 0.5, b = 0.1)

# Solve equations, NOTE the use of method = "lsoda"
output_raw <- ode(y = state, times = times, func = SI_open_model, parms = parameters,
                  method = "lsoda", events = list(func = event_I_cull, root = TRUE), rootfun = root)
# Convert to data frame for easy extraction of columns
output <- as.data.frame(output_raw)

# Plot output
par( mfrow = c(1, 1))
plot( output$time, output$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N),
      xlab = "Time", ylab = "Number")
lines( output$time, output$I, lwd = 2, col = "red")
legend("topright", legend = c("Susceptible", "Infected"),
       lty = 1, col = c("blue", "red"), lwd = 2, bty = "n")

#b) What happens to the infection dynamics when the infected animals are culled?

# Answer:


#c) Assume now that when an infected herd is culled, the same proportion of animals
# is ADDED to the susceptible population. 

# HINT you will need to change the event function to include additions to the S state


##### YOUR CODE GOES HERE #####


#d) . What happens to the infection dynamics when the infected animals are culled?
# How is this different to when only infected animals are culled?

# Answer: 


########## 3- Using Rcpp

## Here we will code our differential equations using Rcpp to compare the speed
# of solving the model. 

## The SIR Rcpp version is an a file called "SIR_model.cpp" and can be sourced as follows
Rcpp::sourceCpp("SIR_model.cpp")

# Open the file in R and look at the formulation of the SIR model in cpp.

# This can be solved in R using deSolve as follows

# Define parameter values
parameters <- c(beta = 0.4, gamma = 0.1)

# Define initial conditions
N <- 100
I_0 <- 1
S_0 <- N - I_0
R_0 <- 0
state <- c(S = S_0, I = I_0, R = R_0)

# Solve equations
output_raw <- ode(y = state, times = times, func = SIR_cpp_model, parms = parameters,
                  method = "rk4")
# Convert to data frame for easy extraction of columns
output <- as.data.frame(output_raw)

# plot results
par(mfrow = c(1, 1))
plot(output$time, output$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N),
     xlab = "Time", ylab = "Number")
lines(output$time, output$I, lwd = 2, col = "red")
lines(output$time, output$R, lwd = 2, col = "green")
legend( "topright", legend = c("Susceptible", "Infected", "Recovered"),
        bg = rgb(1, 1, 1), lty = rep(1, 2), col = c("blue", "red", "green"), lwd = 2, bty = "n")