######################################################
#              ODE's in R: Practical 1               #
######################################################

# Load in the deSolve package
library(deSolve)
# If the package is not installed, install using the install.packages() function

########## 1 - Solving the SI model using deSolve
# The code below will solve the SI model with beta = 0.4 over 50 days with
# initial conditions S(0) = 99, I(0) = 1. Run the following lines including the
# plot() line to create a plot of the output

# Define model function
SI_model <- function(times, state, parms) {
  ## Define variables
  S <- state["S"]
  I <- state["I"]
  N <- S + I
  # Extract parameters
  beta <- parms["beta"]
  # Define differential equations
  dS <- - (beta * S * I) / N
  dI <- (beta * S * I) / N
  res <- list(c(dS, dI))
  return(res)
}

# Define parameter values
parameters <- c(beta = 0.4)

# Define time to solve equations
times <- seq(from = 0, to = 50, by = 1)

# Define initial conditions
N <- 100
I_0 <- 1
S_0 <- N - I_0
state <- c(S = S_0, I = I_0)

# Solve equations
output_raw <- ode(y = state, times = times, func = SI_model, parms = parameters,
                  method = "rk4")
# Convert to data frame for easy extraction of columns
output <- as.data.frame(output_raw)

# Plot output
par(mfrow = c(1, 1))
plot(output$time, output$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N),
      xlab = "Time", ylab = "Number")
lines(output$time, output$I, lwd = 2, col = "red", type = "l")
legend("topright", legend = c("Susceptible", "Infected"),
       lty = c(1, 1), col = c("blue", "red"), lwd = 2, bty = "n")

# a) Increase the initial number of infected individuals, what happens to the
# output?
# Answer: The number of infecteds as a higher intercept but same asymptote

# b) What does the 'by' argument in the 'times' vector represent?
# Answer: The time steps of the model solution

# c) Increase the value of the "by" argument. What happens to the output?
# HINT: plot using type = "b" to plot both lines and points
# Answer: Solution becomes less precise.


########## 2 - Solving the SIR model using deSolve

# Now, let's solve a Susceptible-Infected-Recovered set of ordinary equations.
# There are now states;  Susceptible, Infected, Recovered
# Once infected people recover at a rate gamma = 0.1
# The code for the model function ONLY is given below.

# Define model function
SIR_model <- function(times, state, parms) {
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
  res <- list(c(dS, dI, dR))
  return(res)
}

# a) To solve this model, you need to write a new vector of parameters and of
# state variables You can assume that there are no recovered individuals at
# time 0 and 1 infected Plot the output of the SIR model with different colours
# for Susceptible, Infected and Recovered individuals

##### YOUR CODE GOES HERE #####

# Define parameter values
parameters <- c(beta = 0.4, gamma = 0.1)

# Define initial conditions
N <- 100
I_0 <- 1
S_0 <- N - I_0
R_0 <- 0
state <- c(S = S_0, I = I_0, R = R_0)

# Solve equations
output_raw <- ode(
  y = state, times = times, func = SIR_model, parms = parameters,
  method = "rk4"
)
# Convert to data frame for easy extraction of columns
output <- as.data.frame(output_raw)

# Plot output
par(mfrow = c(1, 1))
plot(output$time, output$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N),
     xlab = "Time", ylab = "Number")
lines(output$time, output$I, lwd = 2, col = "red")
lines(output$time, output$R, lwd = 2, col = "green")
legend("topright", legend = c("Susceptible", "Infected", "Recovered"),
        lty = 1, col = c("blue", "red", "green"), lwd = 2, bty = "n")


# b). Change the value of the transmission rate so that the basic reproduction
# number, # is less than one, i.e. R_0 < 1, what happens to the output?
# Hint:
#   -- Recall that for an SIR model, the basic reproduction number,
#      R_0 = beta / gamma

# Answer:  The epidemic does not take off

##### YOUR CODE GOES HERE #####

parameters <- c(beta = 0.09, gamma = 0.1)

output_raw <- ode(
  y = state, times = times, func = SIR_model, parms = parameters,
  method = "rk4"
)
# Convert to data frame for easy extraction of columns
output <- as.data.frame(output_raw)

# plot results
par(mfrow = c(1, 1))
plot(output$time, output$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N),
      xlab = "Time", ylab = "Number")
lines(output$time, output$I, lwd = 2, col = "red")
lines(output$time, output$R, lwd = 2, col = "green")
legend("topright", legend = c("Susceptible", "Infected", "Recovered"),
       lty = 1, col = c("blue", "red", "green"), lwd = 2, bty = "n")


########## 3 - Solving the SEIR model using deSolve

# The code below has been written to solve a
# Susceptible-Exposed-Infected-Recovered model. In the model individuals can be
# in one of four states. Once infected, susceptible individuals move to the
# exposed class. Exposed individuals become infectious at a rate delta = 0.14

# If you attempt to run the code below, you will receive an error when ode() is
# called.

# a) Fix the SEIR_model function so that the ode() line runs without errors.

# Answer:
# E is missing in the preamble and the return
# No value given for new delta parameter in params vector

# # Define model function
SEIR_model <- function(times, state, parms) {
  ## Define variables
  S <- state[1]
  E <- state[2]
  I <- state[3]
  R <- state[4]
  N <- S + E + I + R
  # Extract parameters
  beta <- parms["beta"]
  gamma <- parms["gamma"]
  delta <- parms["delta"]
  # Define differential equations
  dS <- -(beta * S * I) / N
  dE <- (beta * S * I) / N - delta * E
  dI <- delta * E - gamma * I
  dR <- gamma * I
  res <- list(c(dS, dE, dI, dR))
  return(res)
}

# Define parameters
parameters <- c(beta = 0.4, gamma = 0.1, delta = 0.14)

# Define time to run model
times <- seq(from = 0, to = 50, by = 1)

# Define initial conditions
N <- 100
I_0 <- 1
S_0  <- N - I_0
R_0 <- 0
state <- c(S = S_0, E = 0, I = I_0, R = R_0)

# Solve equations
output_raw <- ode(
  y = state, times = times, func = SEIR_model, parms = parameters,
  method = "rk4"
)
# Convert to data frame for easy extraction of columns
output <- as.data.frame(output_raw)

# plot results
par(mfrow = c(1, 1))
plot(output$time, output$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N),
     xlab = "Time", ylab = "Number")
lines(output$time, output$I, lwd = 2, col = "red")
lines(output$time, output$R, lwd = 2, col = "green")
lines(output$time, output$E, lwd = 2, col = "cyan")
legend("topright",
       legend =  c("Susceptible", "Exposed", "Infected", "Recovered"),
       lty = 1, col = c("blue", "red", "green", "cyan"), lwd = 2, bty = "n")


########## Optional questions
########## 4 - Adding vaccination to the SIR model

# a) Extend the SIR model to include a vaccinated class.
# Here we assume that susceptible individuals are vaccinated at a rate v = 0.05
# The vaccine is 100% effective, so once vaccinated, individuals cannot become
# infected

# HINT: you will need to create a new class V, you can assume that the initial
# number of vaccinated individuals is 0

# Define model function
SIRV_model <- function(times, state, parms) {
  ## Define variables
  S <- state["S"]
  I <- state["I"]
  R <- state["R"]
  V <- state["V"]
  N <- S + I + R + V
  # Extract parameters
  beta <- parms["beta"]
  gamma <- parms["gamma"]
  v <- parms["v"]
  # Define differential equations
  dS <- - (beta * S * I) / N - v * S
  dI <- (beta * S * I) / N - gamma * I
  dR <- gamma * I
  dV <- v * S
  res <- list(c(dS, dI, dR, dV))
  return(res)
}


# Define parameter values
parameters <- c(beta = 0.4, gamma = 0.1, v = 0.05)

# Define initial conditions
N <- 100
I_0 <- 1
S_0 <- N - I_0
R_0 <- 0
V_0 <- 0
state <- c(S = S_0, I = I_0, R = R_0, V = V_0)

# Solve equations
output_raw <- ode(
  y = state, times = times, func = SIRV_model, parms = parameters,
  method = "rk4"
)
# Convert to data frame for easy extraction of columns
output <- as.data.frame(output_raw)

# Plot results
par(mfrow = c(1, 1))
plot(output$time, output$S, type = "l", col = "blue", lwd = 2, ylim = c(0, N),
     xlab = "Time", ylab = "Number")
lines(output$time, output$I, lwd = 2, col = "green")
lines(output$time, output$R, lwd = 2, col = "red")
lines(output$time, output$V, lwd = 2, col = "purple")
legend("topright",
        legend = c("Susceptible", "Infected", "Recovered", "Vaccinated"),
        lty = 1, col = c("blue", "red", "green", "purple"), lwd = 2, bty = "n")
