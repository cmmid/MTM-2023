source("../reference.R")

# `refgraphs` is a list of pre-generated graphs

# modify your build network function to instead load
# the relevant pre-generated graph
build_network <- function(
  i
) {
  igfile <- refgraphs[
    i
  ]
  ig <- read_graph(igfile, directed = FALSE)
  V(ig)$state <- "S"
  return(ig)
}

# plot some example reduced graphs to get a feel for what's going on
# with simulation...
plot(build_network(i=1))
plot(build_network(i=2))

# The rest of this code is pasted in from ..._altSIR.R; there is
# actually only one point to modify, annotated by the usual #### <YOUR CODE HERE> ####

# We need to regularly extract the simulation state from an igraph as results
# With the right filter in ..., you can use length(V(ig)[...]) to get the pertinent info
network_state_totals <- function(ig) {
  return(c(
    S = length(V(ig)[state=="S"]), # n.b., if you wanted something a bit more flexible (for example, add an E state), you could do:
    I = length(V(ig)[state=="I"]), # return(sapply(c("S","I","R"), function(st) length(V(ig)[state == st])))
    R = length(V(ig)[state=="R"])  # or if your states were defined as factors instead:
  ))                               # table(V(ig)$state)
}                                  # though factors can be a bit messy

# Like other stochastic simulations, your Reed-Frost SIR simulation will run until
# there would be no state changes.  What should you check for here?
still_infectious <- function(ig) { # n.b. for flexibility, could also re-use network_state_totals here and do something like
  return(length(V(ig)[             # return(sum(network_state_totals(ig)[c("I")]))  
    state == "I"                   # which would allow you to e.g. easily add an "E" compartment
  ])) 
}                                     

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
    i
  )
  
  # initially, all the vertices but one should be susceptible,
  # with that one infectious
  # Aside: does it matter which one is infectious? Answer: Still doesn't matter - random is random
  V(ig)[
    1
  ]$state <- "I"
  
  # this sets aside a data structure to record simulation steps
  # inspect result & rf_prealloc from the Rstudio console prompt to understand
  # that structure better
  # see reference.R for a bit more explanation
  result <- rf_prealloc(n)
  tm <- 1
  
  # We're going to be working the infectious and susceptible individuals
  # recall from the warmup how to list vertices for an igraph (or ?igraph::V)
  # and how to get only the ones that have a particular attribute (in our case "state")
  # run the Reed-Frost simulation
  
  while(still_infectious(ig)) { # while there are still infectives...
    result[
      tm,
    ] <- network_state_totals(ig) # update results
    
    infective_individuals <- V(ig)[
      state == "I"
    ] # get a vertix list of all the Is
    susceptible_individuals <- V(ig)[
      state == "S"
    ] # and similar for getting all the Ss
    
    # In this approach, the we have pre-removed the edges that won't transmit;
    # So how should we identify transmission paths now?
    transmitting_paths <- E(ig)[infective_individuals %--% susceptible_individuals]
    
    new_infections <- susceptible_individuals[.inc(transmitting_paths)] # from the transmission paths, identify which individuals will become infectious
    
    # now update the simulation state
    V(ig)[
      infective_individuals
      ]$state <- "R"
    V(ig)[
      new_infections
      ]$state <- "I"
    tm <- tm + 1
    
  }
  result[
    tm,
  ] <- network_state_totals(ig) # record final step
  
  # return the results, after trimming them with a function from reference.R
  return(rf_trim(result))
}

resultplot <- plotter(
  simulator_A = igraph_sim, # your simulator...
  simulator_B = chainbinom_sim, # the reference simulation
  samples = 100, # how many times to run the two sims
  n = 50, p = .05 # the Reed-Frost model parameters: population size, and transmission probability
) # note: parameters not adjustable this time, since the background graphs are fixed

print(resultplot)