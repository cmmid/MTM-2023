
#' REMINDER: Your TODOs are marked with Q: ...
#' There are some items marked with SIDEQ: ...
#' Do NOT consider those until you've gotten to the end of the practical
#' and A'd all the Q's.

source("reference.R")

#' MTM Networks Practical 1 population builder
#' 
#' @param N, the population size of the network
#' @param ..., other arguments to be ignored
#'   (defined this way, so it can be used interchangeably with other practicals)
#' @return an [igraph] graph
#' 
#' @examples
#' pop <- build_network_1(30, 0.1)
#' plot(pop, vertex.label = NA, vertex.color = SIRcolors[V(pop)$state], edge.width = 0.5) 
build_network_1 <- function(N, ...) {
  #' make a network where everyone is connected
  ig <- make_full_graph(N, directed = FALSE)
  #' start out everyone as susceptible
  V(ig)$state <- "S"
  #' then introduce infection to one individual. SIDEQ: does it matter which?
  V(ig)[1]$state <- "I"
  #' randomly draw a transmission value for each edge. SIDEQ: how many times does
  #' a particular edge between individuals need to be considered for transmission?
  E(ig)$draw <- runif(ecount(ig))
  #' define edge attribute for showing transmission
  E(ig)$active <- FALSE
  return(ig)
}

#' ALT if we go with library version
#' Examine the `build_network_1` function
#' ?build_network_1
build_network_1

#' Q: Recalling the definitions from the introductory and Networks MTM sessions,
#' what Reed Frost model *variables* & *parameters* appear in `build_network_1`?
#' Which aspects of the Reed-Frost model are represented here?
#' 
#' A:
#' variables: S & I (no R yet)
#' parameters: N (no p)

#' given an SIR network population, what is the next state under Reed-Frost model?
#' 
#' @param network, the population; an [igraph] graph with vertex "state" attribute
#'   in set {S, I, R}
#' @param p, the probability of transmission from an infectious individual
#' @return an update to the input network
#' 
#' @examples 
next_state_1 <- function(network, p) {
  delta <- network
  #' all infectious & susceptible
  infectious_individuals <- V(delta)[state == "I"]
  susceptible_individuals <- V(delta)[state == "S"]
  # all infectious individuals will recover
  V(delta)[infectious_individuals]$change <- "R"
  E(delta)$active <- FALSE #' whatever happened previously now over
  
  if (length(susceptible_individuals)) {
    #' all the potential transmission routes
    infection_paths <- E(delta)[infectious_individuals %--% susceptible_individuals]
    #' all the realized transmission routes - compare draw to p
    transmitting_paths <- infection_paths[draw < p]
    if (length(transmitting_paths)) {
      new_infections <- susceptible_individuals[.inc(transmitting_paths)]
      E(delta)[transmitting_paths]$active <- TRUE
      V(delta)[new_infections]$change <- "I"
    }
  }
  
  return(delta)
}

#' Q: What Reed Frost variables & parameters are needed for state update?
#' A: Now need all the variables (S, I, & R), as well as p from parameters.
#' Unlike some versions of SIR, we do NOT consider the population size N,
#' during the update, but it is implicitly present, via what links exist


#' Q: Think back to the idea of a "model iteration loop" concept motivating
#' earlier sessions, how does Reed-Frost work? Which of the loop constructs,
#' `for` vs `while` is appropriate? Why?
#' 
#' Check your intuition against `?run_network_reed_frost` and the function used
#' in it's loop condition.
#' 
#' A: Use a `while` loop, and compute as long as there are any infectious individuals

#' Now let's consider an example run of the Reed-Frost model implemented on a
#' network:
sim_example <- run_network_reed_frost(
  #' the Reed-Frost transmission probability
  p = 0.1,
  # setup a population
  current_network = build_network_1(N=30),
  # function to calculate the next state in the model iteration loop
  deltafun = next_state_1
)

#' we can look at the resulting epidemic in summary
#' :
state_example <- flatten_network_storage(sim_example)
state_example













#' produce animation of network record along side a state record time series
plot_network_series <- function(sim_output) {
  init <- sim_output[[1]]
  pl <- layout_(init, with_fr(coords = layout_as_star(init)), normalize())
  colnames(pl) <- c("vx", "vy")
  epairs <- as_edgelist(init)
  starts <- pl[epairs[,1],]
  colnames(starts) <- paste0(colnames(starts), ".start")
  ends <- pl[epairs[,2],]
  colnames(ends) <- paste0(colnames(ends), ".end")
  e.ref <- as.data.table(cbind(starts, ends))
  v.ref <- as.data.table(pl)
  e.active <- rbindlist(lapply(
    sim_output, function(net) e.ref[E(net)[active == TRUE], ]
  ), idcol = "time")[, time := time - 1L ]
  v.states <- rbindlist(lapply(
    sim_output, function(net) v.ref[, .(vx, vy, state = V(net)$state) ]
  ), idcol = "time")
  
  geom_net <- function(tm) list(
    geom_segment(
      aes(vx.start, vy.start, xend = vx.end, yend = vy.end, color = "inactive"),
      e.ref,
      size = 0.25, alpha = 0.5
    ),
    geom_segment(
      aes(vx.start, vy.start, xend = vx.end, yend = vy.end, color = "active", group = time),
      e.active[time %in% tm],
      size = 0.75, alpha = 1
    ),
    geom_point(
      aes(vx, vy, color = state, size = state, group = time),
      v.states[time %in% tm]
    ),
    scale_color_manual(guide = "none", values = c(SIRcolors, c(active="red", inactive="grey"))),
    scale_size_manual(guide = "none", values = c(S=1, I=3, R=1))
  )
  
  return(ggplot() + geom_net(1) + coord_equal() + theme_minimal() + theme(
    axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title = element_blank(), panel.grid = element_blank(),
    legend.position = "none"
  ))
  
}

#' first do a single network to get a feel what's conceptual framework

#' step 2, do a bunch of samples, look at duration + final size plot

samples.dt <- sample_reed_frost(n=1000, N=30, p=0.1, setupfun = build_network_1, deltafun = next_state_1)

plot_dur_size(samples.dt)


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

