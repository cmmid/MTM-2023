
build_network <- function(N, p) {
  
  ig <- make_full_graph(N)
  V(ig)$state <- "S" ## need to update

  n_edges <- ecount(ig)
  remove_edges <- E(ig)[ (runif(n = n_edges) > p) ]
  ig <- delete_edges(ig, remove_edges)
  
  return(ig)
  
}

#' Q: what are the Reed Frost variables & parameters represent in `build_network`?
#' A: variables: S and I (R = N - S - I)
#'    parameters: N and p

state_update <- function(network) {
  
  # Identify current S and I individuals (nodes)
  susceptible_individuals <- V(network)[state == "S"]
  infected_individuals <- V(network)[state == "I"]
  
  # Identify S-I edges
  transmitting_paths <- E(network)[susceptible_individuals %--% infected_individuals]
  
  # Newly infected nodes
  new_infections <- susceptible_individuals[.inc(transmitting_paths)]
  
  # Update states
  ## infected -> recovered
  V(network)[infected_individuals]$state <- "R"
  ## susceptible -> infected
  V(network)[new_infections]$state <- "I"
  
  return(network)
  
}

#' Q: What Reed Frost variables & parameters are needed for state update?
#' Added R to variables and now need p for parameters (but no longer explicitly thinking in terms of N)

still_infectious <- function(network) {
  
  number_infections <- sum(V(network)$state == "I")
  
  return(number_infections > 0)
  
}

#' Q: In Reed Frost, we have the step where all infectious individuals interact with susceptibles.
#' Thinking in terms of a loop, what kind should we use? Or put another way, what is the stopping
#' condition for running a Reed-Frost model?
#' A: are there any infectious individuals left?

run_reed_frost <- function(N, p) {
  
  network <- build_network(N, p)
  V(network)[1]$state <- "I"
  
  network_record <- list(network)
  while(still_infectious(network)) {
    
    changes <- state_update(network)
    network_record <- c(changes, network_record)
    network <- network_record[[1]]
    
  }
  
  return(network_record)
}

state_record <- function(network) {
  
  number_S <- sum(V(network)$state == "S")
  number_I <- sum(V(network)$state == "I")
  number_R <- sum(V(network)$state == "R")
  
  state <- c(number_S, number_I, number_R)
  
  return(state)
  
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

