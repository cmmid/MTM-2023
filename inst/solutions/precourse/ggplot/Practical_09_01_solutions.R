##########################################################
# Processing outputs                                     #
##########################################################

# Consider an extension to the SIR model from the lecture where a disease,
# languorem secundus, has two stages of infection. Infected individuals are
# only infectious during the first phase of infection. Mortality follows only
# after the second phase of infection.
#
# 100 simulations have been performed in order to understand the disease's
# prevalence and incidence in the population. A CSV file has been provided for
# each of the four populations in the model:
#  S  - susceptible
#  I1 - first stage of infection
#  I2 - second stage of infection
#  R  - deceased
#
# The model used to simulate the disease is
f_siir <- function(time, state, parameters) {
    
    with(as.list(c(state, parameters)), {
        
        infections1 <- beta*S*I1
        infections2 <- gamma*I1
        deaths      <- delta*I2
        
        dS  <- -infections1
        dI1 <-  infections1 - infections2
        dI2 <-  infections2 - deaths
        dR  <-                deaths
        
        return(list(c(dS, dI1, dI2, dR)))
    })
}
#
#
# Each file contains the following columns:
#  time   - the time point in the simulation (in years)
#  key    - which population is contained within this file
#  1..100 - a vector of population densities for that population

#############################
# A. Reading in data
#############################

# A.1 Read in the CSV files and convert them to a single wide format data frame
library(tidyverse)
library(pracma)

S  <- read_csv("S.csv")
I1 <- read_csv("I1.csv")
I2 <- read_csv("I2.csv")
R  <- read_csv("R.csv")

all_dat <- bind_rows(S, I1, I2, R) # pull together into one data frame

#############################
# B. Plotting a single simulation
#############################

# B.1 Select the time and key columns along with one of the simulation runs.
# Use the `group`, `color` or `fill` aesthetic with the `key` column to produce
# a visualisation of all four populations of interest, S, I1, I2, R. You may
# choose any plotting geometry you consider appropriate from the documentation:
#
# https://ggplot2.tidyverse.org/reference/index.html

# Potential Answers:
# filled, stacked, area plot
all_dat %>%
    dplyr::select(time, key, value = `76`) %>%
    dplyr::mutate(key = fct_inorder(key)) %>%
    ggplot(data =., aes(x=time, y=value)) +
    geom_area(aes(fill = key)) + theme_bw() + 
    xlab("Time (days)") + ylab("Population density") +
    theme(legend.position = "bottom") +
    scale_fill_discrete(name = "Population of interest")

# small multiples of filled area plot
all_dat %>%
    dplyr::select(time, key, value = `76`) %>%
    dplyr::mutate(key = fct_inorder(key)) %>%
    ggplot(data =., aes(x=time, y=value)) +
    geom_line() + facet_wrap(~key) +
    geom_area(aes(fill = key)) + theme_bw() + 
    xlab("Time (days)") + ylab("Population density")  +
    theme(legend.position = "bottom") +
    scale_fill_discrete(name = "Population of interest")

# small multiples of line plots
all_dat %>%
    dplyr::select(time, key, value = `76`) %>%
    dplyr::mutate(key = fct_inorder(key)) %>%
    ggplot(data =., aes(x=time, y=value)) +
    geom_line() + facet_wrap(~key) + theme_bw() + 
    xlab("Time (days)") + ylab("Population density")

# coloured line plots all on one axis
all_dat %>%
    dplyr::select(time, key, value = `76`) %>%
    dplyr::mutate(key = fct_inorder(key)) %>%
    ggplot(data =., aes(x=time, y=value)) +
    geom_line(aes(color = key)) + theme_bw() + 
    xlab("Time (days)") + ylab("Population density") +
    theme(legend.position = "bottom") +
    scale_color_discrete(name = "Population of interest")

#############################
# C. Calculating the prevalence and incidences
#############################

# C.1 Reshape the data frame from wide (100 simulations side by side) to long
# format (stacked atop each other). This so we can repeatedly apply the 
# calculations to all simulations at once.
#
# NB: we want to gather only on the columns labelled X1, X2 ... X100, not 
# on the time and key.

all_dat_long <- gather(all_dat,
                       key = simulation, # copied from column names
                       value = value,    # name of new "value" column
                       -c(time, key))    # don't gather on time or key

# C.2 Read in the parameters used to simulate the population trajectories. We
# need these values so that we can do a join with the simulations and calculate
# the prevalence and incidence post hoc (remembering best practice is to do
# this during the model simulation

parameters <- read_csv("100_simulations_parms.csv")

# C.3 Convert the long data frame to wide, so that we have the following 
# headings: time, simulation, S, I1, I2, R

all_dat_parameters <- all_dat_long %>%  
    spread(key = key, value = value) %>% # S/I1/I2/R become column names
    mutate(simulation = parse_number(simulation)) %>% # so it's not character
    arrange(simulation) %>%              # not necessary, pleasing to the eye
    inner_join(parameters)               # copy in relevent parameter values

# C.4 Calculate the following new variables:
# N.B. Check the model structure where appropriate
#   N     - the sum of non-deceased individuals
#   prev1 - fraction of the non-deceased population with stage 1 disease
#   prev2 - fraction of the non-deceased population with stage 2 disease
#   Inc1  - the rate at which new stage 1 infections are occurring
#   Inc2  - the rate at which new stage 2 infections are occurring

all_dat_parameters <- all_dat_parameters %>%
    mutate(N     = S + I1 + I2, # all non-deceased
           prev1 = I1/N,        # prevalence is infected/total
           prev2 = I2/N,        # prevalence is infected/total
           Inc1  = beta*S*I1,   # raw rate of new stage 1 infections
           Inc2  = gamma*I1)    # raw rate of new stage 2 infections

# C.5 Using the `select()` function, make a data frame that contains the time,
# simulation index & our two new incidence variables, the initial S population,
# S0. Gather this data frame so that the two incidence variable are stacked 
# atop each other so that we can calculate incidence rates as incidence/S0 and
# cumulative incidence as the cumulative integral of these incidence rates

all_cumulative_incidences <- 
    all_dat_parameters %>%
    select(time,          # we want the plot time series
           simulation,    # ...for each simulation
           Stage1 = Inc1, # ...keeping the incidence rates for stage 1
           Stage2 = Inc2, # ...and 2 of the disease
           S0) %>%        # ...and the initial susceptible population size
    
    gather(key = Stage,         # we will make a new column called Stage
           value = Incidence,   # ...and the value of incidence at each stage
           Stage1, Stage2) %>%  # ...can be found in the Stage1 and Stage2 cols
    
    group_by(simulation, Stage) %>%
    mutate(inc_Rate = Incidence/S0,             # per capita rate of infection
           cum_Inc  = cumtrapz(time, inc_Rate)) # prop. of S(0) been in I1/I2

# C.6 Group the simulated cumulative incidences by disease stage and time, 
# then calculate the median and 90% interval for the cumulative incidences.
# Plot these quantities to show the uncertainty in the cumulative incidence

summarised_cumulative_incidences <- 
    all_cumulative_incidences %>%
    group_by(time, Stage) %>%  # for each disease stage at each time
    summarise(median = median(cum_Inc), # ...calculate summary statistics
              lower  = quantile(cum_Inc, 0.05),
              upper  = quantile(cum_Inc, 0.95))

ggplot(data = summarised_cumulative_incidences,
       aes(x=time)) +
    geom_line(aes(y = median)) +
    geom_line(aes(y = lower), lty=2) +
    geom_line(aes(y = upper), lty=2) +
    facet_wrap(~Stage, ncol = 1) + theme_bw() +
    xlab("Time (years)") + 
    ylab("Cumulative incidence")

