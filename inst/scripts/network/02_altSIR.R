
require(MTM)
require(igraph)

reminder(
"This material is written using the `igraph` library. There are other libraries
that provide the same basic functionality, but via different approaches,
e.g. `networkx`."
)

#' In the previous practical, we used networks, but there wasn't
#' actually much structure - each individual in the population was
#' connected to everyone else. That effectively results in the same
#' outcome as the Reed-Frost model *without* any network structure.
#'
#' In this exercise, we will consider randomly percolated networks:

pars <- list(N=30, p=0.05)
pars |> network_build() -> previous_example
network_percolate(pars, previous_example) -> sim_example

list(list(
  "Reference Reed-Frost\nNetwork" = network_quickplot(previous_example),
  "Percolated\nNetwork" = network_quickplot(sim_example)
)) |> patchwork_grid()

#' @question What differs between [network_build()] and [network_percolate()]?
#'
#' @question variables: S and I (R = N - S - I)
#'    parameters: N and p

#' Q: What Reed Frost variables & parameters are needed for state update?
#' Added R to variables, but not longer explicitly need N or p

state_update <- function(network, ...) {
  delta <- network
  # Identify current S and I individuals (nodes)
  infectious_individuals <- V(delta)[state == "I"]
  susceptible_individuals <- V(delta)[state == "S"]
  # all infectious individuals will recover
  V(delta)[infectious_individuals]$change <- "R"
  E(delta)$active <- FALSE #' whatever happened previously now over

  if (length(susceptible_individuals)) {
    # Identify S-I edges
    transmitting_paths <- E(network)[susceptible_individuals %--% infected_individuals]

    if (length(transmitting_paths)) {
      # Newly infected nodes
      new_infections <- susceptible_individuals[.inc(transmitting_paths)]
      E(delta)[infection_paths]$active <- TRUE
      V(delta)[new_infections]$change <- "I"
    }
  }

  return(delta)
}

apply_changes <- function(network, delta) {
  changedv <- V(delta)[!is.na(change)]
  V(network)[changedv]$state <- changedv$change
  E(network)$active <- E(delta)$active
  return(network)
}

still_infectious <- function(network) any(V(network)$state == "I")

#' Q: In Reed Frost, we have the step where all infectious individuals interact with susceptibles.
#' Thinking in terms of a loop, what kind should we use? Or put another way, what is the stopping
#' condition for running a Reed-Frost model?
#' A: are there any infectious individuals left?

run_reed_frost <- function(N, p) {
  network <- build_network(N, p)
  network_record <- list(network)
  while(still_infectious(network)) {
    delta <- state_update(network, p)
    network <- apply_changes(network, delta)
    network_record <- c(list(network), network_record)
  }
  return(rev(network_record))
}

state_record <- function(network, statelevels = c("S", "I", "R")) {
  setNames(sapply(statelevels, function(s) length(V(network)[state == s])), statelevels)
}

convert_to_state_record <- function(network_record) {
  s <- sapply(network_record, state_record)
  s <- rbind(t = 1:dim(s)[2], s)
  as.data.table(t(s))
}

sample_reed_frost <- function(N, p, n) rbindlist(
  lapply(1:n, function(i) {
    set.seed(i);
    return(convert_to_state_record(run_reed_frost(N, p)))
  }), idcol = "sample"
)

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

