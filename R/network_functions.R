#' @import igraph data.table ggplot2 gganimate patchwork

network_check_Np <- function(N, p) stopifnot(
  "'N' must be an integer." = is.integer(N) || (N == as.integer(N)),
  "'N' must be a scalar." = length(N) == 1,
  "'N' must be positive." = N > 0,
  "'p' must be numeric." = is.numeric(p),
  "'p' must be a probability." = between(p, 0, 1),
  "'p' must be a scalar." = length(p) == 1
)

#' @title base SIR network for networks exercises
#'
#' @param N, a positive integer; the population size
#' @param p, a numeric on 0 to 1; the probability of transmission between
#' individuals per time step
#'
#' @details Note, this step actually performs all the relevant random number
#' draws, therefore to get matching results (e.g. to compare with other stduents)
#' the random number seed must be set ahead of calling this
#'
#' @return an igraph object, with N vertices, all connected by undirected edges
#'
#' @export
network_build <- function(
  N, p
) {
  network_check_Np(N, p)
  network <- make_full_graph(N, directed = FALSE) # make a network where everyone is connected

  # define model states
  network$states <- c("S", "I", "R")
  network$inf_states <- "I"

  V(network)$state <-    "S" # start out everyone as Susceptible
  V(network)[1]$state <- "I" # ... except set one Infectious individual.
  # ASIDE: does it matter which individual is set to "I"?
  # ASIDE: We used `V(network)$state <- "S"` because its more obvious for beginners
  # what that means. However, why might e.g. `V(network)$state <- network$states[1]`
  # be preferable? Where else in this function definition does that sort of
  # pattern apply?


  # randomly draw a transmission value for each edge.
  E(network)$draw <- runif(ecount(network)) <= p
  # ASIDE: how many times does a particular connection between individuals need
  # to be considered for transmission?

  # define edge attribute for showing transmission
  E(network)$active <- FALSE

  return(network)
}

#' @title transforms a network by percolation (selectively keeping / removing edges)
#'
#' @param ig, an igraph object; the base network, it must have a graph attribute
#' 'p' which is a probability, and edge attribute 'draw'
#'
#' @return a new igraph object, with potentially fewer edges. it's 'p' attribute
#' will be == 1
#'
#' @export
network_percolate <- function(
  network
) {
  stopifnot(
    "'ig' is an igraph." = is.igraph(network),
    "'ig' has a graph attribute 'p'." = "p" %in% graph_attr_names(network),
    "'ig' graph attribute 'p' is a probability." = between(network$p, 0, 1),
    "'ig' edges all have attribute 'draw', a probability" = all(between(E(network)$draw, 0, 1))
  )
  remove_edges <- E(network)[ draw < (1-network$p) ]
  network$p <- 1
  return(delete_edges(network, remove_edges))
}

#' not exported; only for checking, which course participants should have to
#' engage with
#'
#' @param ig the object to check; want: an igraph, with pertinent properties
#' @return NULL
network_check <- function(
  network
) stopifnot(
  is.igraph(network),
  "state" %in% names(vertex_attr(network)),
  all(vertex_attr(network, "state") %in% network$states),
  all(c("draw", "active") %in% names(edge_attr(network)))
)

#' not exported; only for checking, which course participants should have to
#' engage with
#'
#' @param ig the object to check; want: an igraph, with pertinent properties
#' @return NULL
network_check_changes <- function(ig) stopifnot(
  is.igraph(ig),
  "change" %in% names(vertex_attr(ig)),
  all(vertex_attr(ig, "change", V(ig)[!is.na(change)]) %in% ig$states)
)

#' @title compute a new network from initial states & changes
#'
#' @param network an igraph, the initial network state
#' @param changes an igraph, the vertex states that change & edges that were
#'   activated leading to those changes
#'
#' @return a new igraph, representing the state changes applied
#'
#' @examples
#' population <- network_build_basic(N = 30, p = 0.1)
#' # TODO
#'
#' @export
network_update <- function(
  network, changes
) {
  # get the subset of vertices with changed state
  changedv <- V(changes)[!is.na(change)]
  # if there are any updates ...
  if (length(changedv)) {
    # apply them to original network
    V(network)[changedv]$state <- changedv$change
    E(network)$active <- E(changes)$active
  }
  return(network)
}

#' @title check if there any infectious individuals in network
#'
#' @param network an igraph, the network
#'
#' @return a logical scalar
#'
#' @examples
#' population <- network_build_basic(N = 30, p = 0.1)
#' # TODO
#'
#' @export
network_is_infectious <- function(
  network
) {
  return(length(V(network)[state %in% network$inf_states]) > 0)
}

#' @title given an SIR network population, what is the next state under Reed-Frost model?
#'
#' @param network, the population; an [igraph] graph with vertex "state" attribute
#'   in set {S, I, R} and graph "p" attribute
#'
#' @return an update to the input network
#'
#' @examples
#' population <- network_build_basic(N = 30, p = 0.1)
#'
#'
#' @export
network_next_state <- function(
  network
) {
  delta <- network
  # the individuals that are infectious or susceptible,
  # as igraph vertex sets
  infectious <- V(delta)[state == "I"]
  susceptible <- V(delta)[state == "S"]
  # all infectious individuals will recover
  V(delta)[infectious]$change <- "R"
  # whatever happened previously now over
  E(delta)$active <- FALSE

  # if there are infectious & susceptible individuals
  if (length(susceptible) & length(infectious)) {
    # find all the transmission routes
    # first find potential paths; `%->%` selects all edges from
    # a left-hand-side vertex set (i.e. infectious)
    # to a right-hand-side vertex set (i.e. susceptible)
    # then, within the possible transmission routes, a subset will occur randomly
    transmitting_paths <- E(delta)[
      infectious %->% susceptible
    ][
      draw <= delta$p
    ]

    # there are any transmitting paths
    if (length(transmitting_paths)) {
      # infect all the individuals at the ends of those paths
      new_infections <- susceptible[.inc(transmitting_paths)]
      E(delta)[transmitting_paths]$active <- TRUE
      V(delta)[new_infections]$change <- "I"
    }
  }

  return(delta)
}



#' @title the algorithm to simulate Reed-Frost model on a network
#'
#' @param initial_network, an [igraph] network, the initial state of the population
#' @param update_fun, a function which receives an [igraph] network and `p`, and returns
#'   a network with vertex attribute `change`, indicating which vertices change,
#'   and edge attribute `active`, indicating which edges contributed to transmission
#' @return a list of [igraph]s, where the list entries correspond to the population
#'   state at time t (e.g. `list[[1]]` is the initial network state after introduction)
#'
#' This function takes the Reed Frost model parameters (N, p) and implementation
#' of the model (set by setupfun, deltafun). Using those, it performs the model
#' iteration loop, and returns the series networks
#'
#' @examples
#' # guarantee a network with some transmission
#' set.seed(42)
#'
#' # using the practical 1 Reed-Frost model definition
#' networkstates <- run_network_reed_frost(
#'   p = 0.1,
#'   network_current = build_network_1(N = 30),
#'   deltafun = next_state_1
#' )
#'
#' reflayout <- layout_nicely(networkstates[[1]])
#' quickplot <- function(state) plot(
#'   state, layout = reflayout, vertex.color = SIRcolors[V(state)$state],
#'   edge.color = c("grey", "red")[E(state)$active + 1L],
#'   vertex.label = NA, edge.width = 0.5
#' )
#'
#' quickplot(networkstates[[1]])
#' quickplot(networkstates[[2]])
#' quickplot(networkstates[[3]])
#' quickplot(networkstates[[4]])
#'
#' @export
network_run_reed_frost <- function(
  initial_network,
  update_fun = network_next_state
) {
  # initialize storage
  current_network <- initial_network
  network_storage  <- list(current_network)
  while(network_is_infectious(current_network)) {
    delta <- update_fun(current_network)
    current_network <- network_update(current_network, delta)
    network_storage <- c(list(current_network), network_storage)
  }
  return(rev(network_storage))
}

#' @title flatten a network into its states
#'
#' @export
network_flatten <- function(network) return(
  table(factor(V(network)$state, levels = network$states, ordered = TRUE))
)

#' @title flatten a simulated series of networks
#'
#' @export
network_flatten_run <- function(network_run) {
  return(as.data.table(t(rbind(t = 1:length(network_run), sapply(network_run, network_flatten)))))
}

#' @title sample a series of Reed-Frost SIR simulations
#'
#' @export
network_sample_reed_frost <- function(
  n,
  N, p,
  setup_fun = network_build,
  update_fun = network_next_state,
  ref.seed = 0
) rbindlist(
  lapply(1:n, function(i) {
    set.seed(i+ref.seed);
    return(network_flatten_run(network_run_reed_frost(setup_fun(N, p), update_fun)))
  }), idcol = "sample"
)

#' @title plot summary of a series of simulations
#'
#' @export
network_plot_histograms <- function(s.dt) {
  # assert: s.dt is ordered by t, and last entry by sample corresponds to end o
  # assert: samples is 1:N
  ref.dt <- s.dt[,.(duration=t[.N]-1, final_size = R[.N]), keyby=sample]
  samples <- ref.dt[, sample[.N]]

  heat.dt <- ref.dt[ , .N, by=.(duration, final_size) ]
  durahist.dt <- ref.dt[, .N, keyby=.(duration)]
  sizehist.dt <- ref.dt[, .N, keyby=.(final_size)]

  max.den <- ceiling(max(durahist.dt$N, sizehist.dt$N)/samples*10)/10

  sx <- scale_x_continuous("Duration", breaks = function(ls) seq(0, ls[2], by=4))
  sy <- scale_y_continuous("Final Size", breaks = function(ls) seq(0, ls[2], by=10))
  sdenx <- scale_x_continuous("Density", breaks = function(ls) seq(0, max.den, by=0.1))
  sdeny <- scale_y_continuous("Density", breaks = function(ls) seq(0, max.den, by=0.1))
  geom_dens <- function(fill = "grey60", stat = "identity", width = 1, ...) geom_bar(fill=fill, stat=stat, width=width, ...)

  p.heat <- ggplot(heat.dt) + aes(duration, final_size, fill = N/samples) +
    geom_tile(alpha = 0.7) +
    sx + sy +
    coord_cartesian(xlim = c(0, NA), ylim = c(0, NA), expand = FALSE) +
    scale_fill_distiller("Density", palette = "Reds", direction = 1) +
    theme_minimal() +
    theme(legend.position = c(.95, 0.05), legend.justification = c(1,0))

  p.dur <- ggplot(durahist.dt) + aes(duration, N/samples) +
    geom_dens() + sx + sdeny +
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    coord_cartesian(xlim=c(0, NA), ylim = c(0, max.den), expand = FALSE)

  p.sz <- ggplot(sizehist.dt) + aes(N/samples, final_size) +
    geom_dens(orientation = "y") + sdenx + sy +
    theme_minimal() + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
    coord_cartesian(xlim=c(0, max.den), ylim = c(0, NA), expand = FALSE)

  # Used for testing only, please delete
  # sampler_output <- sampler(num_samples = 1000, simfunc = igraph_sim, observef = rf_observer, n = 50, p = 0.06)

  p.tot <- p.dur + plot_spacer() + p.heat + p.sz +
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

  return(p.tot)

}

#' @title plot the time series samples
#'
#' @export
network_plot_series <- function(s.dt) {
  if (!"sample" %in% colnames(s.dt)) {
    s.dt <- copy(s.dt)[, sample := 1 ]
  }
  alph <- s.dt[, 1/sqrt(length(unique(sample))) ]
  wide.dt <- melt(s.dt, id.vars = c("sample", "t"))
  return(ggplot(wide.dt) +
    aes(t, value, color = variable, group = interaction(sample, variable)) +
    geom_line(data = function(dt) dt[variable == "S"], alpha = alph) +
    geom_line(data = function(dt) dt[variable == "R"], alpha = alph) +
    geom_line(data = function(dt) dt[variable == "I"], alpha = alph) +
    scale_x_continuous(name = "Simulation time") +
    scale_y_continuous(name = NULL) +
    scale_color_manual(name = NULL, values = SIRcolors, guide = guide_legend(override.aes = list(alpha = 1))) +
    theme_minimal() +
    theme(legend.position = c(1, 0.5), legend.justification = c(1, 0.5))
  )
}

network_animate_series <- function(sim_output) {

}

plot_network_series <- function(sim_output) {
  init <- sim_output[[1]]
  pl <- layout_(init, with_fr(coords = layout_as_star(init)), normalize())
  colnames(pl) <- c("vx", "vy")
  epairs <- as_edgelist(init)
  starts <- pl[epairs[,1],]
  colnames(starts) <- paste0(colnames(starts), ".start")
  ends <- pl[epairs[,2],]
  colnames(ends) <- paste0(colnames(ends), ".end")
  e.ref <- as.data.table(cbind(starts, ends))[, eid := 1L:.N ]
  v.ref <- as.data.table(pl)[, vid := 1L:.N ]
  e.active <- rbindlist(lapply(
    c(sim_output, sim_output[length(sim_output)]), function(net) e.ref[E(net)[active == TRUE], ]
  ), idcol = "time")[, time := time - 1L ]
  v.states <- rbindlist(lapply(
    sim_output, function(net) v.ref[, .(vid, vx, vy, state = V(net)$state) ]
  ), idcol = "time")

  geom_net <- function(tm) list(
    geom_segment(
      aes(vx.start, vy.start, xend = vx.end, yend = vy.end, color = "inactive"),
      e.ref,
      size = 0.25, alpha = 0.5
    ),
    geom_segment(
      aes(vx.start, vy.start, xend = vx.end, yend = vy.end, color = "active", group = eid),
      e.active[time %in% tm],
      size = 0.75, alpha = 1
    ),
    geom_point(
      aes(vx, vy, color = state, size = state, group = vid),
      v.states[time %in% tm]
    ),
    scale_color_manual(guide = "none", values = c(SIRcolors, c(active="red", inactive="grey"))),
    scale_size_manual(guide = "none", values = c(S=1, I=3, R=1))
  )

  return(ggplot() +
           geom_net(1:length(sim_output)) +
           transition_time(time) +
           coord_equal() + theme_minimal() + theme(
             axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
             axis.title = element_blank(), panel.grid = element_blank(),
             legend.position = "none"
           ))

}

