## This small script shows us Latin Hypercube Sampling vs random Monte Carlo sampling
library(lhs)
# Pick some small number of samples
n <- 100
# First we're going to sample 100 times from a random sample
mc_unif <- runif(n)
# 100 lh samples across 1 parameter
latin_unif <- randomLHS(n, 1) 

#plot these two distribution
dev.off()
par(mfrow=c(3,2))
hist(mc_unif)
hist(latin_unif)

# You can see how the Latin Hypercube does a great job of sampling evenly across the distribution

# Let's now sample from a Normal distribution using a random monte carlo sample across the whole distribution
mc_norm <- rnorm(n, mean = 0, sd = 1)

# How do we sample using an LHS? 
# We use the previous numbers generated from the uniform LHS to draw samples from the Normal using the Inverse Cumulative Sampling
latin_norm <- qnorm(latin_unif, mean = 0, sd = 1)


#plot these two normal distributions
hist(mc_norm)
hist(latin_norm)

# the latin hypercube sample looks much better! 
# Why does this work?

# first let's look at the norm probability distibution
x <- seq(-6,6, by =0.1) #random variable X
normdens <- dnorm(x, mean = 0, sd = 1) # prob distribution, f(X)
normcumul <- pnorm(x, mean = 0, sd = 1) # cumulative distribution, F(X)
plot(x, normdens, "l")
plot(x, normcumul, "l")

# Most of the density is in the middle range of values (-1 to 1). 
# So we want a method to sample from this more often than the other areas in the distribution.
# Specifically, we want to sample values from X proportionally to the probability of those values occuring
# Let's generate some samples between 0-1. These can be values on our Y-axis. Then, if we ask what is the value of the cumulative distribution that corresponds to these uniform values we are taking the inverse

# for illustration let's just choose 10 points
ex_latin <- randomLHS(10, 1) 
# which X values are given by using these as the Y value (denoted by "X"s)?
abline(h = ex_latin, col = "red")
points(qnorm(ex_latin, mean = 0, sd = 1), y=rep(0,10), pc = "x")
# You can see that the samples are clustered around the middle:
# in areas of X with higher density, the gradient of the cumluative distribution (F(X)) will be very steep, causing more values between 0 and 1 to map to this range of X with high density
# That is, F^{-1}(R) = X where R is a uniform random number between 0 and 1.

# So, you can sample from any distribution whose cumluative function is 'invertable' by plugging in uniform random numbers to the inverse cumulative function of your new distribution
# for more information check out: https://en.wikipedia.org/wiki/Inverse_transform_sampling

# Likewise, to perform LHS on a uni- or mulitvariate non-uniform distribution, we can transform our LHS samples from a uniform distribution as above.
