### Practical 9 - Visualisation 1 ###

# Consider an SIR model. 
#
# A CSV file has been provided for each of the three populations in the model, 
# for each of 100 simulations:
#  S  - susceptible
#  I  - infectious
#  R  - recovered
#
# The model used to simulate the disease is
f_sir <- function(time, state, parameters) {
    
    with(as.list(c(state, parameters)), {
        
        infections  <- beta*S*I
        deaths      <- gamma*I
        
        dS <- -infections 
        dI <-  infections - deaths
        dR <-               deaths
        
        return(list(c(dS, dI, dR)))
    })
}
# where beta is the transmission rate (per person, per day) and gamma is the
# recovery rate (by day)

############################# PREPARING DATA FOR PLOTTING ######################

# a) Load the relevant packages and read in the CSV file
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

all_dat <- read_csv("beta_1.56756_gamma_0.36508.csv")
all_dat
# should return four columns: | time | S | I | R |

all_dat_long <- pivot_longer(all_dat, 
                             cols = c(S, I, R),
                             names_to = "state",
                             values_to = "proportion")

all_dat_long
# should return three columns: | time | state (key) | proportion (value) |


############################# PLOTTING RESHAPED DATA ###########################

# b) With your long data frame, make a plot that shows how the proportion of 
# the population in each state changes over time. Use the line geometry and 
# color each line by state. You might find the following link useful
#
# https://ggplot2.tidyverse.org/reference/index.html

ggplot(data = all_dat_long, 
       aes(x = time,
           y = proportion)) +
    geom_line(aes(color = <YOUR CODE HERE>)) 

# c) Modify the levels of the state variable to be in S,I,R order rather than
# in alphabetical order. Copy and paste the code from the previous plot and re-
# run it to see how the ordering of the states has changed

all_dat_long$state <- factor(all_dat_long$state,
                             levels = c("S", "I", "R"))

plot_SIR <- ggplot(data = all_dat_long, 
                   aes(<YOUR CODE HERE>) +
                geom_line(<YOUR CODE HERE>) 

plot_SIR

# d) Add labels to the x and y axes, change to theme_bw() and set the position
# of the legend to be at the bottom of the plot

plot_SIR + 
    theme_bw() + 
    xlab("<YOUR CODE HERE>") + 
    ylab("<YOUR CODE HERE>") +
    theme(legend.position = "bottom") 

# e) Instead of colour, use small multiples to show how each state changes over
# time
plot_SIR_faceted <- ggplot(data = all_dat_long,
       aes(x = time,
           y = proportion)) +
    geom_line() + 
    theme_bw() + 
    xlab("Time (days)") + 
    ylab("Population proportion") +
    facet_wrap(facets = vars(state)) 

plot_SIR_faceted