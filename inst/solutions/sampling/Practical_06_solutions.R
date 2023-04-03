
### Practical 10 - Sensitivity and Sampling ###

########################### (1) ONE-WAY UNCERTAINTY ANALYSIS ##############################
# First, let's run an ODE model.
# Open up the SIRmodel.R file

# (a) Which functions are in here?
# Answer: solveODE() and SIR_model()
# (b) What are the arguments of the first function?
# Answer: (1) the parameter values lists, (2) Argument to plot everything, (3) number of row to plot (4) number of cols to plot
# (c) What is the output of the first function?
# Answer: maximum prevalence through the epidemic

# First let's clear our workspace, remove plots and load the libraries we need
rm(list=ls())
dev.off()
library(deSolve)


# Let's read in these functions so we have them to hand
source("SIRmodel.R")

# Let's choose a beta value of 0.4 and a gamma value of 0.2
max.prevalence = solveODE(parameters <- c(beta = 0.4, gamma = 0.2))
print(max.prevalence)

# Now let's look at the effect of the maximum prevalence of the epidemic across gamma = 0.1 -1.0 (increment on 0.1)
# fill in the for-loop to make this happen

gamma.vec = seq(0.1, 1.0, by = 0.1)
max.prevalence = vector()
  
for (gamma.val in gamma.vec){
  mp = solveODE(parameters = c(beta = 0.4, gamma = gamma.val),
                                         plot.all.results = FALSE)
  max.prevalence = c(max.prevalence, mp)
}

# Now we have our max.prevalence, we need to plot this against our infectiousness duration

# plot max.prevalence as a function of the infectiousness duration
par(new=FALSE)
par(mfrow=c(1,1))
plot(1/gamma.vec, max.prevalence, type = "b", 
     xlab = "Infectiousness Duration (days)",
     ylab = "Maximum Prevalence",
     main= "One-way uncertainty analysis")


## Now try to increase the resolution of gamma to get a better idea of the relationship

# Replace gamma.vec = seq(0.1, 1.0, by = 0.1) with
inf.duration = 1:10
gamma.vec = 1/inf.duration
# but remember to clear max.prevalence first!


# (d) Describe in words the qualitative relationship
# Answer: There is no epidemic until the infectiousness duration is >2 (R0>1)
# after that there is a linear increase in the maximum prevalence until gamma = 6,
# then there is a diminishing increase in maximum prevlance



###################### (2) MONTE CARLO SAMPLING ######################

# Now suppose that we have a previous epidemiological study that suggested that R0 has a mean value of 5, 
# but uncertainty within the range of -1, +1
# However, we still don't know whether the infectiousness period is 1 day or 10 days. 
# We will now use the functions in SIRmodel_R0.R to make a similar plot as above, but this time, 
# incorporate the uncertainty of R0 for each discrete value of gamma

# We're going to first use a direct Monte Carlo Sampling method

# Read in our set of functions in SIRmodel_R0.r
source("SIRmodel_R0.R")
# First, let's set a fixed seed for the random number generator
# this will allow us to run the code again and retrieve the same 'simulation'

set.seed(2019)

# Now, draw R0 1,000 times from a suitable distribution (e.g. normal)

r0.all = rnorm(1000, 5, 0.5)
size.df = length(r0.all) * length(gamma.vec)
# initialise max.prevalence again, this time it needs to be a dataframe or a matrix
max.prevalence = data.frame(r0.value = vector(mode= "numeric", length = size.df), 
                            gamma = vector(mode= "numeric", length = size.df), 
                            max.prev = vector(mode= "numeric", length = size.df))
index = 0
# create a loop over each of these R0 values in turn
for (r0.val in r0.all){
  
  # create a loop over each of these Gamma values in turn
  for (gamma.val in gamma.vec){
      index = index + 1     
      mp = solveODE_2(parameters = c(R0 = r0.val, gamma = gamma.val))
      max.prevalence[index, "r0.value"] = r0.val
      max.prevalence[index, "gamma"] = gamma.val
      max.prevalence[index, "max.prev"] = mp
  }
}

# Take a look at max.prevalence by using the 'head() function
head(max.prevalence)
# (e) How have we saved the output?
# Answer: using 'long' formatting -- see the next practical?

# Now plot this output using the R function MCplot() in SIRmodel_R0.R
MCplot(max.prevalence)

# (f) What conclusions can you draw from the plot?
# Answer: Increasing the rate of recovery reduces the max prevalence
# However, the uncertainty in R0 has a larger impact on the maximum prevalence than infectious duration
# In fact, until the infectiousness duration decreases below 1/0.3 (3d), this parameter doesn't affect prevalence

# (g) Why does the plot look different to the one you plotted previously?
# Final exercise:  We have used a combination of scripts and function to complete this plot. 
# (g) Write out the description of how we could plot this output ONLY using functions (optimal)
# Answer: Write out the main components of the code:
 # 1. Pick which parameters and how they will be sampled / sequenced
 # 2. Generate an outcome measure from the parameter values
 # 3. Plot each outcome measure as a function of the parameter values
# (h) Next add to this outline by splitting these descriptions into functions
# MAIN FUNCTION
  # > calls SAMPLING FUNCTION (output set of samples / sequences of parameters)
  # > calls ODE SOLVE FUNCTION (input output from SAMPLING, output variable of interest)
  # > calls PLOT FUNCTION (input= output variable from ODE solve function; no output)
# These 4 components could be in their own source files. Sampling, ODE solve and Plot might all call other functions (located in the same file)

# (h) Translate this outline to actual code - notice how there is a trade-off between making your code 
 # quick to write and making it generalisable to other situations
