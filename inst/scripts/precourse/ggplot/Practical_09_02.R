### Practical 9 - Visualisation 2 ###

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

############################# MULTIPLE SIMULATIONS #############################

# Load the relevant packages and read in the CSV file
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# a) Read in the 100 simulation data set, gather it to long format and make
# state into a factor variable

all_dat_100 <- read_csv("100_simulations_wide.csv")

all_dat_100
# should return five columns: | sim | time | S | I | R |

all_dat_100_long <- pivot_longer(all_dat_100, 
                                 cols = <YOUR CODE HERE>,
                                 names_to = <YOUR CODE HERE>,
                                 values_to = <YOUR CODE HERE>)

all_dat_100_long
# should return four columns: | sim | time | state (key) | proportion (value) |


all_dat_100_long$state <- <YOUR CODE HERE> 

# b) Plot all 100 simulations from the SIR model 
# Hint: you will need to use the group aesthetic with your line and may choose
# to set the lines to be semi-transparent. Use faceting (small multiples) to 
# show each state in its own subplot; faceting will be easier in all plots from
# this point on than colouring by state

ggplot(data = all_dat_100_long,
       aes(x = time, 
           y = proportion)) +
    geom_line(aes(group = sim), alpha = 0.05) +
    theme_bw() + 
    facet_grid(<YOUR CODE HERE>) +
    xlab("Time(days)") +
    ylab("Proportion of population")

############################# CALCULATING SUMMARY STATISTICS ###################

# c) Use the group_by() function to tell R that we want to calculate summary
# statistics for each state at each time

all_dat_100_long_grouped <- group_by(all_dat_100_long,
                                     state, time) 

all_dat_100_long_grouped
# should return four columns: | sim | time | state (key) | proportion (value) |
# should also let you know there are 303 groups

# d) Use the summarise function to calculate the median and 95% interval for
# each state at each time point. You will need to calculate all three summary
# statistics separately, and will do so within the same summarise() 

all_dat_100_long_summarised <- 
    summarise(all_dat_100_long_grouped,
              q0.025 = quantile(proportion, probs = 0.025),
              q0.500 = quantile(proportion, probs = 0.5),
              q0.975 = quantile(proportion, probs = 0.975))


# e) Use geom_ribbon to plot the 95% interval and geom_line to plot the median
# for each state. For its aesthetics, geom_ribbon requires a ymin and ymax and
# can be coloured and filled and made semi-transparent.
# Ensure you label the axes appropriately.

ggplot(data = all_dat_100_long_summarised,
       aes(x = time)) +
    geom_ribbon(aes(ymin = q0.025,  # lower edge of ribbon
                    ymax = q0.975), # upper edge of ribbon
                alpha = 0.5,   # make semi-transparent
                fill = "lightskyblue", # fill blue
                color = NA) +     # no border color
    geom_line(aes(y = q0.500)) +       # line for median
    facet_grid(cols = vars(state)) +
    theme_bw() +               # nicer theme
    xlab("Time (days)") +      # human friendly axis label
    ylab("Population")        # human friendly axis label


# f) If you have time left, you may wish to investigate visualising all 100
# simulations, colouring by state, as before, and faceting by simulation

ggplot(data = all_dat_100_long,
       aes(x = time)) +
    geom_line(aes(y = proportion, color = state)) +
    facet_wrap(facets = vars(sim)) +
    theme_bw() +               # nicer theme
    xlab("Time (days)") +      # human friendly axis label
    ylab("Population") +       # human friendly axis label
    theme(legend.position = "bottom")

# g) Discuss whether you think faceting by state or simulation gives a clearer
# understanding of how the simulations vary