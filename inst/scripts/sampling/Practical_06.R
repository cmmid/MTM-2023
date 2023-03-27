
### Practical 10 - Sensitivity and Sampling ###

########################### (1) ONE-WAY UNCERTAINTY ANALYSIS ##############################
# First let's clear our workspace, remove plots and load the libraries we need
rm(list=ls())
dev.off()
library(MTM)
library(deSolve)

# Examine the pertinent functions for this step of practical
sampling_dSIR_betagamma
sampling_maxprevalence

# (a) What is the first function?
# Answer: a step function for the SIR model, parametrised by beta and gamma,
# (b) What are the arguments of the second function?
# Answer: (1) the parameter values lists, (2) whether or not to plot results, (3) which model to use
# (c) What is the output of the second function?
# Answer: maximum prevalence through the epidemic

# Let's choose a beta value of 0.4 and a gamma value of 0.2
max.prevalence = sampling_maxprevalence(parameters = list(beta = 0.4, gamma = 0.2), plot_results = TRUE)
print(max.prevalence)

# Now let's look at the effect of the maximum prevalence of the epidemic across gamma = 0.1 -1.0 (increment on 0.1)
gamma.vec <- #### <YOUR CODE HERE> ####
# initialise max.prevalence container (to the same size as gamma.vec)
max.prevalence = numeric(length(gamma.vec))

# Wrap this in a loop to fill in the max.prevalence vector
#### <YOUR CODE HERE> ####
  max.prevalence[i] = sampling_maxprevalence(
    parameters = list(beta = 0.4, gamma = gamma.vec[i])
  )
#### <YOUR CODE HERE> ####

# Now we have our max.prevalence, we need to plot this against our infectiousness duration

# plot max.prevalence as a function of the *infectiousness duration*
par(new=FALSE)
par(mfrow=c(1,1))
plot(#### <YOUR CODE HERE> ####, #### <YOUR CODE HERE> ####, type = "b",
     xlab = "Infectiousness Duration (days)",
     ylab = "Maximum Prevalence",
     main= "One-way uncertainty analysis")

## Now try to increase the resolution of gamma to get a better idea of the relationship
# but remember to clear max.prevalence first!

#### <YOUR CODE HERE> ####




# (d) Describe in words the qualitative relationship
# Answer:



###################### (2) MONTE CARLO SAMPLING ######################

# Now suppose that we have a previous epidemiological study that suggested that R0 has a mean value of 5,
# but uncertainty within the range of -1, +1
# However, we still don't know whether the infectiousness period is 1 day or 10 days.
# We will now use the functions in SIRmodel_R0.R to make a similar plot as above, but this time,
# incorporate the uncertainty of R0 for each discrete value of gamma

# We're going to first use a direct Monte Carlo Sampling method
# Let's look at the different model step function we'll be using:
sampling_dSIR_gammaR0
# note: we're using the relationship between R0 and beta here,
# then re-using the sampling_dSIR_betagamma function

# First, let's set a fixed seed for the random number generator
# this will allow us to run the code again and retrieve the same 'simulation'

set.seed(2019)

# Now, draw R0 1,000 times from a suitable distribution (e.g. normal)
r0.all = #### <YOUR CODE HERE> ####

# initialise max.prevalence again, this time it needs to be a dataframe or a matrix
# `expand.grid` is convenient function to give you all combinations of some values
# as a `data.frame`
max.prevalence <- expand.grid(r0.value = r0.all, gamma = gamma.vec)
# now add a column to-be-filled-in
max.prevalence$max.prev <- NA_real_

# create a loop over all the samples, and evaluate
# the max prevalence at each
# n.b. this will take a minute
for (sample_index in 1:dim(max.prevalence)[1]) {
  max.prevalence$max.prev[sample_index] <- sampling_maxprevalence(
    parameters = list(
      R0 = max.prevalence$r0.value[sample_index],
      gamma = max.prevalence$gamma[sample_index]
    ),
    dModel = sampling_dSIR_gammaR0
  )
}

# Take a look at max.prevalence by using the 'head() function
head(max.prevalence)
# (e) How have we saved the output?
# Answer:

# Now plot this output using the R function MTM::sampling_MC_plot()
sampling_MC_plot(max.prevalence)

# (f) What conclusions can you draw from the plot?
# Answer:

# (g) Why does the plot look different to the one you plotted previously?

# Final exercise:  We have used a combination of scripts and function to complete this plot.
# (g) Write out the description of how we could plot this output ONLY using functions (optimal)
# Answer:

# (h) Translate this outline to actual code - notice how there is a trade-off between making your code
 # quick to write and making it generalisable to other situations
