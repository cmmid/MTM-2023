
require(MTM)

split_network_build <- function(N, p) {
  n1 <- as.integer(N/2)
  n2 <- N-n1
  e1 <- (n1^2-n1)/2
  e2 <- (n2^2-n2)/2
  etot <- (N^2-N)/2
  # (e1 + e2)*psub + 1 == etot*p =>
  psub <- max(min((etot*p-1)/(e1 + e2), 1), 0)

  net1 <- network_percolate(network_build(n1, psub))
  net2 <- network_percolate(network_build(n2, psub))


}

partition_network <- function(N, p) {
  n1 <- as.integer(N/2)
  n2 <- N-n1

  ig1 <- percolate_graph(make_full_graph(n1), psub)
  ig2 <- percolate_graph(make_full_graph(n2), psub)

  igcombo <- add_edges(ig1 + ig2, c(sample(n1, 1), sample(n2, 1)+n1))

  V(igcombo)$state <- "S"
  V(igcombo)[1]$state <- "I"
  E(igcombo)$active <- FALSE

}

#' we're going to need to know the number of edges
#' in a full graph. could get it from the igraph objects
#' but it's also a succinct formula
full_graph_ecount <- function(n) (n^2-n)/2
#' ASIDE: nice maths refresher puzzle as to why this
#' is true. Can approach from nothing by using some
#' summation insights, or as confirmation exercise
#' using induction.

#' going to reuse the same approach as in practical 2
percolate_graph <- function(ig, p) {
  remove_edges <- E(ig)[ runif(ecount(ig)) < (1-p) ]
  return(delete_edges(ig, remove_edges))
}

build_network <- function(N, p) {
  #' building two network partitions & then combining them
  n1 <- as.integer(N/2)
  n2 <- N-n1

  #' these are going to be connected by a single edge,
  #' and accounting for all these non-connections, we still
  #' want the overall connection probability to be `p` on average
  e1 <- full_graph_ecount(n1)
  e2 <- full_graph_ecount(n2)
  etot <- full_graph_ecount(N)
  psub <- min((etot*p-1)/(e1 + e2), 1)

  ig1 <- percolate_graph(make_full_graph(n1), psub)
  ig2 <- percolate_graph(make_full_graph(n2), psub)

  igcombo <- add_edges(ig1 + ig2, c(sample(n1, 1), sample(n2, 1)+n1))

  V(igcombo)$state <- "S"
  V(igcombo)[1]$state <- "I"
  E(igcombo)$active <- FALSE

  return(igcombo)
}

#' identical to version from practical 2
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

run_reed_frost <- function(N, p) {
  network <- build_network(N)
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

#' DEMO: start with comparison of several graph snapshots
#' Q: what do you notice about these networks?
#' How do you expect these difference(s) to be reflected
#' in the final size / duration distributions?

#' DEMO: do a bunch of samples, look at duration + final size plot
#' Q: What do you notice about these distributions? Does that comport
#' with the differences you expected?

#' want to elicit that there is extinction (close to zero final size lump) + there are outbreaks (bigger, non-zero lump)
#' and those vary in size + relationship to duration of epidemic (generally larger => longer?)

