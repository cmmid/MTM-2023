
### Practical: Sensitivity and Sampling ###

###################### (1) ONE-WAY UNCERTAINTY ANALYSIS ########################
# First let's clear our workspace, remove plots and load the libraries we need
rm(list = ls())
dev.off()
library(MTM)
library(deSolve)

# Examine the pertinent functions for this step of practical
sampling_dSIR_betagamma
sampling_maxprevalence

# (a) What is the first function?
# Answer:
# (b) What are the arguments of the second function?
# Answer:
#
# (c) What is the output of the second function?
# Answer:

# Let's choose a beta value of 0.4 and a gamma value of 0.2
max.prevalence <- sampling_maxprevalence(
  parameters = list(beta = 0.4, gamma = 0.2), plot_results = TRUE
)
print(max.prevalence)

# Now let's look at the effect of the maximum prevalence of the epidemic across
# gamma = 0.1 - 1.0 (increment on 0.1)
gamma.vec <- #### <YOUR CODE HERE> ####
# initialise max.prevalence container (to the same size as gamma.vec)
max.prevalence <- numeric(length(gamma.vec))

# Wrap this in a loop to fill in the max.prevalence vector
#### <YOUR CODE HERE> ####
  max.prevalence[i] <- sampling_maxprevalence(
    parameters = list(beta = 0.4, gamma = gamma.vec[i])
  )
#### <YOUR CODE HERE> ####

# Now we have our max.prevalence, we need to plot this against our
# infectiousness duration:

# plot max.prevalence as a function of the infectiousness duration
par(new = FALSE)
par(mfrow = c(1, 1))
plot(#### <YOUR CODE HERE> ####, #### <YOUR CODE HERE> ####, type = "b",
     xlab = "Infectiousness Duration (days)",
     ylab = "Maximum Prevalence",
     main = "One-way uncertainty analysis")

## Now try to increase the resolution of gamma to get a better idea of the
# relationship but remember to clear max.prevalence first!

#### <YOUR CODE HERE> ####



# (d) Describe in words the qualitative relationship
# Answer:
#
#

###################### (2) MONTE CARLO SAMPLING ######################

# Now suppose that we have a previous epidemiological study that suggested that
# R0 has a mean value of 5, but uncertainty within the range of -1, +1.
# However, we still don't know whether the infectiousness period is 1 day or
# 10 days. We will now use the functions in SIRmodel_R0.R to make a similar plot
# as above, but this time, incorporate the uncertainty of R0 for each discrete
# value of gamma.

# We're going to first use a direct Monte Carlo Sampling method
# Let's look at the different model step function we'll be using:
sampling_dSIR_gammaR0
# note: we're using the relationship between R0 and beta here,
# then re-using the sampling_dSIR_betagamma function

# First, let's set a fixed seed for the random number generator
# this will allow us to run the code again and retrieve the same 'simulation'

set.seed(2019)

# Now, draw R0 1,000 times from a suitable distribution (e.g. normal)
r0.all <- #### <YOUR CODE HERE> ####

# initialise max.prevalence again, this time it needs to be a dataframe or a
# matrix `expand.grid` is convenient function to give you all combinations of
# some values as a `data.frame`
max.prevalence <- expand.grid(r0.value = r0.all, gamma = gamma.vec)
# now add a column to-be-filled-in
max.prevalence$max.prev <- NA_real_

# create a loop over all the samples, and evaluate
# the max prevalence at each
# n.b. this will take a minute
for (sample_index in seq_len(dim(max.prevalence)[1])) {
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
#
#
#

###################### (3) LHS vs MONTE CARLO SAMPLING #########################

## First let's load in the library we'll need for later

library(lhs)

# We're going to first sample directly from a full distribution uniform
# distribution from 0 to 1. How many samples will we need to take?

# Let's try a few options and see how well they do
par(mfrow = c(3, 2))
n <- c(10, 100, 1000, 10000, 20000)
rn <- n |> lapply(rnorm)
tmp <- rn |> mapply(
  hist, x = _, main = sprintf("Histogram of rnorm(n = %i)", n), xlab = NA
)

# Now let's plot the sample size against the variance of the sample distribution
varn <- rn |> lapply(var)
plot(n, varn,
     ylab = "variance", main = "Variance of sampled normal"
)
abline(h = 1)

### Let's now use 100 samples to see the difference between a Monte Carlo
# sampling and a LHS sampling approach. Pick some small number of samples:
n <- 100

# First we're going to sample 100 times from a random sample
mc_unif <- runif(n)
# 100 lh samples across 1 parameter
latin_unif <- randomLHS(n, 1)

#plot these two distribution
dev.off()
par(mfrow = c(3, 2))
hist(mc_unif)
hist(latin_unif)

# You can see how the Latin Hypercube does a great job of sampling evenly across
# the distribution

# Let's now sample from a Normal distribution using a random Monte Carlo sample
# across the whole distribution
mc_norm <- rnorm(n, mean = 0, sd = 1)

# How do we sample using an LHS?
# We use the previous numbers generated from the uniform LHS to draw samples
# from the Normal using the Inverse Cumulative Sampling
latin_norm <- qnorm(latin_unif, mean = 0, sd = 1)


#plot these two normal distributions
hist(mc_norm, xlim = c(-6, 6))
hist(latin_norm, xlim = c(-6, 6))

# the latin hypercube sample looks much better!
# Why does this work?

# first let's look at the norm probability distibution
x <- seq(-6, 6, by = 0.1) #random variable X
normdens <- dnorm(x, mean = 0, sd = 1) # prob distribution, f(X)
normcumul <- pnorm(x, mean = 0, sd = 1) # cumulative distribution, F(X)
plot(x, normdens, "l")
plot(x, normcumul, "l")

# Most of the density is in the middle range of values (-1 to 1).
# So we want a method to sample from this more often than the other areas in the
# distribution. Specifically, we want to sample values from X proportionally to
# the probability of those values occuring. Let's generate some samples
# between 0-1. These can be values on our Y-axis. Then, if we ask what is the
# value of the cumulative distribution that corresponds to these uniform values
# we are taking the inverse

# for illustration let's just choose 10 points
ex_latin <- randomLHS(10, 1)
# which X values are given by using these as the Y value (denoted by "X"s)?
abline(h = ex_latin, col = "red")
points(qnorm(ex_latin, mean = 0, sd = 1), y = rep(0, 10), pc = "x")
# You can see that the samples are clustered around the middle:
# in areas of X with higher density, the gradient of the cumluative distribution
# (F(X)) will be very steep, causing more values between 0 and 1 to map to this
# range of X with high density/ That is, F^{-1}(R) = X where R is a uniform
# random number between 0 and 1.

# So, you can sample from any distribution whose cumluative function is
# 'invertable' by plugging in uniform random numbers to the inverse cumulative
# function of your new distribution
# For more details: https://en.wikipedia.org/wiki/Inverse_transform_sampling

# Likewise, to perform LHS on a uni- or mulitvariate non-uniform distribution,
# we can transform our LHS samples from a uniform distribution as above.
