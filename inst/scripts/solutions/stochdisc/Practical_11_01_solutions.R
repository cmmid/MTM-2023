# A model of sensitive and resistant Streptococcus pneumoniae competition in the nasopharynx
library(ggplot2) # For plotting

# A. Model parameters: events
g <- 0.02     # "Germ size" of a transmitted strain [0.02]
m <- 20.0     # Growth constant of S. pneumoniae within host [20.0]
b <- 0.05     # Within-host growth "benefit" of sensitive strain [0.05]

# B. Model parameters: granularity of simulation
dt <- 1/30   # Length of each time step in months [1/30]
years <- 1   # Number of years to simulate [1]

##### The total number of time steps to run the simulation.
##### Hint: this can be calculated based on years, dt, and the number of months per year.
steps <- (12 * years)/dt

# C. Events
# Here, we define, in functional form, all the events that can happen to a host.
# Each function takes a vector (s, r) of the host's sensitive and resistant strain carriage
# and evaluates to a vector of the same format.

# Exposure to sensitive strain
ExposeToS <- function(h) {
    list(s = h$s + g, r = h$r)
}

##### Fill in the other 4 needed event functions below:
ExposeToR <- function(h) {
    list(s = h$s, r = h$r + g)
}

Clearance <- function(h) {
    list(s = 0, r = 0)
}

Treatment <- function(h) {
    list(s = 0, r = h$r)
}

Growth <- function(h) {
    ds_dt <- h$s * m * (1 + b - h$s - h$r)
    dr_dt <- h$r * m * (1 - h$s - h$r)
    list(s = h$s + ds_dt * dt, r = h$r + dr_dt * dt)
}

##### D. Test of events
##### Do each of these events do what you expect them to do?
test_host <- list(s = 0.5, r = 0.5)
test_host
ExposeToS(test_host)
ExposeToR(test_host)
Clearance(test_host)
Treatment(test_host)
Growth(test_host)

##### E. The simulation for dynamics within an individual host
##### We define our host as a list with two properties, s and r,
##### for sensitive and resistant carriage respectively.
host <- list(s = 0, r = 0)

##### We will also want to store the results of our simulation at
##### regular time steps, which we can use a data frame to do.
##### Any parameters passed to data.frame of length 1 (like s and
##### r below) are automatically expanded to be the length of the
##### longest parameter (in this case, time).
results <- data.frame(time = 0:steps * dt, s = 0, r = 0)

##### Before the simulation begins, store the host's properties
##### for the first time step.
results$s[1] <- host$s
results$r[1] <- host$r

# Loop over each time step . . .
for (ts in 1:steps)
{
    ##### Basically, we just want to see if a sequence of events in time
    ##### produces the expected output. So fill in some events below to
    ##### see if it all makes sense.
    ##### There is no single "correct" sequence here, of course!
    if (ts == 1)     host <- ExposeToR(host)
    if (ts == 50)    host <- ExposeToS(host)
    if (ts == 100)   host <- Clearance(host)
    if (ts == 150)   host <- ExposeToS(host)
    if (ts == 150)   host <- ExposeToR(host)
    if (ts == 300)   host <- Treatment(host)
    
    ##### Each time step, the host should be subjected to growth and competition.
    host <- Growth(host)

    ##### Store simulation results.
    results$s[ts + 1] <- host$s
    results$r[ts + 1] <- host$r
}

##### F. Plot simulation results
##### Try using ggplot on the "results" data frame to visualise the dynamics within
##### your host from above.

##### Lots of ways of showing the results -- here are a couple.
ggplot(results) + 
    geom_line(aes(x = time, y = r), colour = "red") + 
    geom_line(aes(x = time, y = s), colour = "blue") +
    labs(x = "Time (months)", y = "Carriage density")

ggplot(results) +
    geom_ribbon(aes(x = time, ymin = 0, ymax = r), fill = "red") + 
    geom_ribbon(aes(x = time, ymin = r, ymax = r + s), fill = "blue") +
    labs(x = "Time (months)", y = "Carriage density")
