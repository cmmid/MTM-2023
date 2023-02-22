
### Practical 10 - Sensitivity and Sampling ###

########################### (1) ONE-WAY UNCERTAINTY ANALYSIS ##############################
# First, let's run an ODE model.
# Open up the SIRmodel.R file

# (a) Which functions are in here?
# Answer: 
# (b) What are the arguments of the first function?
# Answer: 
# (c) What is the output of the first function?
# Answer: 

# First let's clear our workspace, remove plots and load the libraries we need
rm(list=ls())
dev.off()
library(deSolve)

# Let's read in these functions so we have them to hand
#### <YOUR CODE HERE> #### 

# Let's choose a beta value of 0.4 and a gamma value of 0.2
max.prevalence <- solveODE(parameters <- c(beta = 0.4, gamma = 0.2))
print(max.prevalence)

# Now let's look at the effect of the maximum prevalence of the epidemic across gamma = 0.1 -1.0 (increment on 0.1)

gamma.vec <- #### <YOUR CODE HERE> ####
  
# initialise max/prevalence container  
max.prevalence <- vector()

# Add in a loop to make this happen
#### <YOUR CODE HERE> ####
  
  mp <- solveODE(parameters = c(beta = 0.4, gamma = gamma.val),
                                         plot.all.results = FALSE)
  max.prevalence <- c(max.prevalence, mp)
  
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

# Read in our set of functions in SIRmodel_R0.r
#### <YOUR CODE HERE> ####
  
# First, let's set a fixed seed for the random number generator
# this will allow us to run the code again and retrieve the same 'simulation'

set.seed(2019)

# Now, draw R0 1,000 times from a suitable distribution (e.g. normal)
r0.all = #### <YOUR CODE HERE> ####

# initialise max.prevalence again, this time it needs to be a dataframe or a matrix
size.df = length(r0.all) * length(gamma.vec)
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
# Answer: 

# Now plot this output using the R function MCplot() in SIRmodel_R0.R
MCplot(max.prevalence)

# (f) What conclusions can you draw from the plot?
# Answer: 

# (g) Why does the plot look different to the one you plotted previously?

# Final exercise:  We have used a combination of scripts and function to complete this plot. 
# (g) Write out the description of how we could plot this output ONLY using functions (optimal)
# Answer: 

# (h) Translate this outline to actual code - notice how there is a trade-off between making your code 
 # quick to write and making it generalisable to other situations
