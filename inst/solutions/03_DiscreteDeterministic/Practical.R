##########################################################
# Discrete time deterministic models Section 1           #
##########################################################

#############################
#Load packages
library(ggplot2)
library(forcats)
library(tidyr)
#############################


#############################
# A. SIR Model from lecture
#############################

# A.1 Implement the SIR model from the slides and plot the proportion of the
# population in each state over time.

time_sir <- seq(0, 20, by = 1)
y_sir    <- matrix(data = NA,
                   nrow = length(time_sir),
                   ncol = 3)


update_sir <- function(t, y, parms) {
    S <- y[1]
    I <- y[2]
    R <- y[3]

    beta  <- parms["beta"]
    gamma <- parms["gamma"]

    out <- c(- beta*S*I,
             + beta*S*I - gamma*I,
             + gamma*I)

    return(out)
}

parms_sir <- c(beta = 1.3,
               gamma = 0.23)

# initial values at t=0: S = 0.99, I = 0.01, R = 0
y_sir[1, ] <- c(0.99, 0.01, 0)

for (i in 2:nrow(y_sir)){

    y_sir[i,] <- y_sir[i-1,] +
        update_sir(t    = time_sir[i],
                   y    = y_sir[i-1, ],
                   parms= parms_sir)

}

# make plot of proportion of population in each state over time
y_sir_df <- as.data.frame(y_sir)

names(y_sir_df) <- c("Susceptible", "Infected", "Recovered")

y_sir_df <- cbind(time = time_sir, y_sir_df)

#convert to "long" format for plotting using ggplot2
y_sir_long <- pivot_longer(y_sir_df,
                           cols = c(Susceptible, Infected, Recovered),
                           names_to = "state",
                           values_to = "proportion",
                           names_transform = list(state = fct_inorder))

head(y_sir)

ggplot(data  = y_sir_long,
       aes(x = time,
           y = proportion)) +
    geom_step() +
    theme_bw() +
    xlab("Time (days)") +
    ylab("Population proportion") +
    facet_wrap(facets = vars(state))


# a) At approximately what time does the peak in infectious population occur
# and what proportion of the population is infectious?

# After approximately 7 days, the proportion infectious is 0.6

# b) Approximately how long does it take for the susceptibles to go to 0?

# After 13 days, the proportion susceptible is below 0.001

# A.2 Change the mean time spent infectious from 4.35 days to 2 days, keeping
# the rate of transmission the same.

# a) At approximately what time does the peak in infectious population occur
# and what proportion of the population is infectious?

# After 8 days, the proportion infectious is 0.3

# b) Approximately how long does it take for the susceptibles to go to 0?

# After 20 days, the proportion susceptible is still above 0.05


parms_sir <- c(beta = 1.3,
               gamma = 1/2)


y_sir[1, ] <- c(0.99, 0.01, 0)
for (i in 2:nrow(y_sir)){
    y_sir[i,] <- y_sir[i-1,] +
        update_sir(t     = time_sir[i],
                   y     = y_sir[i-1, ],
                   parms = parms_sir)
}

# make plot of proportion of population in each state over time
y_sir_df <- as.data.frame(y_sir)

names(y_sir_df) <- c("Susceptible", "Infected", "Recovered")

y_sir_df <- cbind(time = time_sir, y_sir_df)

#convert to "long" format for plotting using ggplot2
y_sir_long <- pivot_longer(y_sir_df,
                           cols = c(Susceptible, Infected, Recovered),
                           names_to = "state",
                           values_to = "proportion",
                           names_transform = list(state = fct_inorder))

ggplot(data  = y_sir_long,
       aes(x = time,
           y = proportion)) +
    geom_step() +
    theme_bw() +
    xlab("Time (days)") +
    ylab("Population proportion") +
    facet_wrap(facets = vars(state))

# A.4 Change the mean time spent infectious back to 4.35 days and set the
# transmission rate to be half what is has been

# a) At approximately what time does the peak in infectious population occur
# and what proportion of the population is infectious?

# After 14 days, the proportion infectious is approximately 0.31

# b) Approximately how long does it take for the susceptibles to go to 0?

# Even after 20 days, the susceptible group is 0.1 of the population

parms_sir <- c(beta = 1.3/2,
               gamma = 0.23)

y_sir[1, ] <- c(0.99, 0.01, 0)
for (i in 2:nrow(y_sir)){
    y_sir[i,] <- y_sir[i-1,] +
        update_sir(time_sir[i],
                   y_sir[i-1, ],
                   parms_sir)
}

# make plot of proportion of population in each state over time
y_sir_df <- as.data.frame(y_sir)

names(y_sir_df) <- c("Susceptible", "Infected", "Recovered")

y_sir_df <- cbind(time = time_sir, y_sir_df)

#convert to "long" format for plotting using ggplot2
y_sir_long <- pivot_longer(y_sir_df,
                           cols = c(Susceptible, Infected, Recovered),
                           names_to = "state",
                           values_to = "proportion",
                           names_transform = list(state = fct_inorder))

ggplot(data  = y_sir_long,
       aes(x = time,
           y = proportion)) +
    geom_step() +
    theme_bw() +
    xlab("Time (days)") +
    ylab("Population proportion") +
    facet_wrap(facets = vars(state))


#############################
# B. SIR model
#############################

# B.1 Adapt the SIR model in the slides to incorporate birth of new susceptibles
# proportional to the sum of the S I and R populations. Balance these new births
# with deaths from each of the S I and R groups, with both the per capita birth
# and death rates being delta=0.01

new_sir <- function(t, y, parms){
    S <- y[1]
    I <- y[2]
    R <- y[3]

    beta  <- parms["beta"]
    gamma <- parms["gamma"]
    delta <- parms["delta"]

    out <- c(-beta*S*I + delta*(S+I+R) - delta*S,
             + beta*S*I - gamma*I - delta*I,
             + gamma*I - delta*R)

    return(out)
}

new_parms <- c(beta = 1.3, gamma = 0.23, delta = 0.1)

time_sir <- seq(0,20,by=1)
y_sir    <- matrix(data = NA, ncol = 3, nrow = length(time_sir))
y_sir[1, ] <- c(0.99, 0.01, 0)

for (i in 2:nrow(y_sir)){
    y_sir[i,] <- y_sir[i-1,] +
        new_sir(time_sir[i],
                y_sir[i-1, ],
                new_parms)
}

# convert matrix to data.frame
y_sir_df <- as.data.frame(y_sir)

# name each column
names(y_sir_df) <- c("Susceptible", "Infected", "Recovered")

# bind to the time_sir vector
y_sir_df <- cbind(time = time_sir, y_sir_df)

# convert to "long" format for plotting using ggplot2
y_sir_long <- pivot_longer(y_sir_df,
                           cols = c(Susceptible, Infected, Recovered),
                           names_to = "state",
                           values_to = "proportion",
                           names_transform = list(state = fct_inorder))

# produce plot
ggplot(data  = y_sir_long,
       aes(x = time,
           y = proportion)) +
    geom_step() +
    theme_bw() +
    xlab("Time (days)") +
    ylab("Population proportion") +
    facet_wrap(facets = vars(state))


# B.2 Calculate N(t) = S(t) + I(t) + R(t) the total number of alive individuals. Make
# a plot of S(t), I(t), R(t) and N(t). Your function N(t) should be constant at
# 1 for all values of t. If this is not the case, ensure the model contains
# births of new S proportional to N, and deaths of each of S I and R

y_sir_df$Alive <- y_sir_df$Susceptible + y_sir_df$Infected + y_sir_df$Recovered

#convert to "long" format for plotting using ggplot2
y_sir_long <- pivot_longer(y_sir_df,
                           cols = c(Susceptible, Infected, Recovered, Alive),
                           names_to = "state",
                           values_to = "proportion",
                           names_transform = list(state = fct_inorder))


ggplot(data  = y_sir_long,
       aes(x = time,
           y = proportion)) +
    geom_step() +
    theme_bw() +
    xlab("Time (days)") +
    ylab("Population proportion") +
    facet_wrap(facets = vars(state))

# B.2
# a) At approximately what time does the peak in infectious population occur
# and what proportion of the population is infectious?

# After 8 days, the proportion infectious is approximately 0.54

# b) Approximately how long does it take for the susceptibles to go to 0?

# The susceptible population does not go to 0, due to the birth of new
# susceptibles

# B.3
# Discuss what happens to the population of S, I and R over time. Consider the
# parameters of the model, what they represent, and whether the assumptions
# they represent are realistic
#
# S(t): The population decreases to a minimum at about 9 days but the birth of
# new susceptibles prevents it reaching 0. It will continue to increase to a
# stable equilibrium.
#
# I(t): The population increases to a peak and then the recovery process causes
# the population to decrease. The infectious population does not decrease down
# to 0 as before as the new susceptibles will get infected. The long-term
# behaviour is a stable equilibrium.
#
# R(t): The recovered population increases to a peak and then decreases to a
# stable equilibrium as the recovered population die and are replaced with
# susceptibles.
#
# There's an implicit assumption in the model that transmission is not passed
# to newborns; i.e. only susceptibles are born. This is likely a reasonable
# assumption to make for many diseases. As we are dealing the proportion of
# the total population it's reasonable to keep N(t) constant, but the birth
# and death rates may not be reasonable. Instead, we might be best to allow
# them to grow indefinitely (or, if the death rate is higher, decrease to 0).
#
# Additionally, we assume that the entire population is capable of giving
# birth to newborns, and that the disease does not cause a loss of life
# expectancy.
