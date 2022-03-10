
build_network <- function(N) {
  ig <- make_full_graph(n)
  V(ig)$state <- factor("S", levels = c("S", "I", "R"))
  return(ig)
}

#' Q: what are the Reed Frost variables & parameters represent in `build_network`?
#' variables == S & I (and sort of R?)
#' parameters == N - not yet p

state_update <- function(network, p) {
  ...
}

#' Q: What Reed Frost variables & parameters are needed for state update?
#' Added R to variables and now need p for parameters (but no longer explicitly thinking in terms of N)

still_infectious <- function() {
  
}

#' Q: In Reed Frost, we have the step where all infectious individuals interact with susceptibles.
#' Thinking in terms of a loop, what kind should we use? Or put another way, what is the stopping
#' condition for running a Reed-Frost model?
#' A: are there any infectious individuals left?

run_reed_frost <- function(N, p) {
  network <- build_network(N)
  network_record <- list(network)
  while(still_infectious(network)) {
    changes <- state_update(network)
    network_record <- c(apply_changes(network, changes), network_record)
    network <- network_record[[1]]
  }
  return(network_record)
}

state_record <- function(network) {
  ...extract state variables
}

convert_to_state_record <- function(network_record) lapply(network_record, state_record)

plot_network_record <- ...produce animation of network record along side a state record time series

#' first do a single network to get a feel what's conceptual framework

#' step 2, do a bunch of samples, look at duration + final size plot

#' Q: what do you notice about these distributions?
#' want to elicit that there is extinction (close to zero final size lump) + there are outbreaks (bigger, non-zero lump)
#' and those vary in size + relationship to duration of epidemic (generally larger => longer?)

#' ask them to do different things with parameters

#' Q: vary p, while holding N constant - what does that do to distribution?
#' p lower => more in the extinction lump, p higher more in the epidemic lump, and epidemic lump pushed higher (though limited by N)
#' TBD re time

...repeat code from earlier question, but indicate they should change things and run it multiple times to look at pictures

#' Q: vary N, holding p constant - what does that do to distribution?
#' Obviously, larger N => larger final sizes. Less obviously: more epidemics.
#' To do with holding *individual* probability constant & increasing N => increasing probability of tranmission
#' since everyone is connected

...repeat code from earlier question, but indicate they should change things and run it multiple times to look at pictures

#' bonus-y question:
#' Q: what constraint on N and p would you have to impose to retain the same shape of the final size
#' distribution with varying N? Hint: how might you have an R0-like concept in this model?

...provide skeleton not code here


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