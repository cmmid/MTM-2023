# INSTRUCTIONS
#
# We're going to work towards a having a function that will:
# - Receive a few relevant arguments (number of individuals, probability of infection, ...)
# - generate a network corresponding to the Reed-Frost SIR assumptions
# - simulate an SIR epidemic on that network, starting with a single initially infectious individual
#   and ending when there are no more infectious individuals
#
# Then you can use that function with some provided code (from `reference.R`) to
# generate a plot comparing a non-network based implementation to your implementation.
# 
# You should work from top-to-bottom, replacing the `#### <YOUR CODE HERE> ####` with relevant code. Each step
# will have comments providing hints about where to look for more information, and
# you can always ask the instructors, but: the first thing they'll ask you is what you
# learned from the hints, and then if you've asked one of your fellow participants!
# There are suggestions about how to test small parts of your code as you go along, and
# when you're done, you should be able to source this file and see a comparison between
# your simulation and the reference one


# This adds several functions into the workspace that we will use for the summary
# evaluation of your simulator, as well as loading all the necessary packages.
# Aside: feel free to read this file, particularly the `chainbinom_sim`, which
# implements the Reed-Frost model use a chain-binomial formalism.
source("../reference.R")

# For the Reed-Frost network model, we're going to have 
# vertices represent people, and the edges represent contacts between them
# First, you'll want a function that builds the network
# we also want to provide a default state (one of S, I, or R) for the all the individuals
# recall from the previous practical that there are several options:
#  - use one of the convenience constructors; hint recall the make_... functions from the warmup
#  - create an edgelist and build the graph from that; see ?graph.edgelist for hints
#  - create an adjacency matrix and build the graph from that; see ??graph.adjacency for hints
# for the Reed-Frost network, remember everyone connects with everyone - there is a make_... function
# for precisely this case.
build_network <- function(
  #### <YOUR CODE HERE> #### # the necessary arguments to inform creating network
) {
  ig <- #### <YOUR CODE HERE> #### # create the basic graph
  V(ig)$state <- #### <YOUR CODE HERE> #### # set the initial state
  return(ig)
}

# a) Create plots of your network for N=6 and N=30.
# (hint: this should tell you what arguments build_network(...) needs)
# Answer:
plot(build_network(
  #### <YOUR CODE HERE> ####
))
plot(build_network(
  #### <YOUR CODE HERE> ####
))

# With `build_network`, we can make the simulation population, but we also need to
# regularly check it's *state* during the simulation - i.e., how many people are S vs I vs R
# We can do that by counting the vertices that are S vs I vs R
# With the right filter in ..., you can use length(V(ig)[...]) to get the pertinent info
network_state_totals <- function(ig) {
  return(c(
    S= #### <YOUR CODE HERE> ####
    ,I= #### <YOUR CODE HERE> ####
    ,R= #### <YOUR CODE HERE> ####
  ))
}

# b) test that your function returns sensible things
# Answer:
network_state_totals(build_network(
  #### <YOUR CODE HERE> ####
)) == #### <YOUR CODE HERE> ####

# Like other stochastic simulations, your Reed-Frost SIR simulation will run until
# there would be no state changes.  What should you check for here?
still_infectious <- function(ig) {
  return(length(V(ig)[
    #### <YOUR CODE HERE> ####
  ]))
}

# c) test that your function returns sensible things
# Answer:
still_infectious(build_network(
  #### <YOUR CODE HERE> ####
)) == #### <YOUR CODE HERE> ####


# Now you need to fill in this simulation function skeleton. You may 
# Make sure your function n, p, and i arguments, corresponding to
#  n - number of individuals
#  p - transmission probability
#  i - random number seed
# and returns a matrix with three columns and at least two rows
igraph_sim <- function(n, p, i) {
  set.seed(i)
  # create the network using your function + the appropriate arguments from n, p, i
  ig <- build_network(
    #### <YOUR CODE HERE> ####
  )
  
  # initially, all the vertices but one should be susceptible,
  # with that one infectious
  # Aside: does it matter which one is infectious?
  V(ig)[
    #### <YOUR CODE HERE> ####
  ]$state <- #### <YOUR CODE HERE> ####
  
  # this sets aside a data structure to record simulation steps
  # inspect result & rf_prealloc from the Rstudio console prompt to understand
  # that structure better
  # see reference.R for a bit more explanation
  result <- rf_prealloc(n)
  tm <- 1
  
  # We're going to be working with the infectious and susceptible individuals
  # recall from the warmup how to list vertices for an igraph (or ?V)
  # and how to get only the ones that have a particular attribute (in our case "state")
  # run the Reed-Frost simulation
  
  while(still_infectious(ig)) { # while there are still infectives...
    result[tm,] <- network_state_totals(ig) # store the current state of the population
    
    infective_individuals <- V(ig)[
      #### <YOUR CODE HERE> ####
    ] # get a vertex list of all the Is
    susceptible_individuals <- V(ig)[
      #### <YOUR CODE HERE> ####
    ] # and similar for getting all the Ss
    
    # If you know a set of "source" vertices (infectious individuals) and possible
    # "target" vertices (susceptible individuals), you can use indexing functions
    # (see ?E then follow the link on indexing for more info) to get
    # all the edges between them, which represent the possible transmission paths
    infection_paths <- E(ig)[
      #### <YOUR CODE HERE> ####
    ]

    transmitting_paths <- #### <YOUR CODE HERE> #### # randomly sample infection_paths to see which edges transmitted infections
    new_infections <- #### <YOUR CODE HERE> #### # from the transmission paths, identify which individuals will become infectious
    
    # now update the simulation state
    V(ig)[
      #### <YOUR CODE HERE> ####
    ]$state <- "R"
    V(ig)[
      #### <YOUR CODE HERE> ####
    ]$state <- "I"
    tm <- #### <YOUR CODE HERE> ####
    
  }
  result[
    #### <YOUR CODE HERE> ####
  ] <- network_state_totals(ig) # record final step
  
  # return the results, after trimming them with a function from reference.R
  return(rf_trim(result))
}

# The `plotter` function is defined in reference
resultplot <- plotter(
  simulator_A = igraph_sim, # your simulator
  samples = 100, # how many times to run the two sims
  n = 50, p = .05 # the Reed-Frost model parameters: population size, and transmission probability
)

print(resultplot)