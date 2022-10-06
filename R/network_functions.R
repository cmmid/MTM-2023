#' @import igraph data.table ggplot2 gganimate patchwork
NULL

assert_SIRgraph <- function(network) {
  stopifnot(
    "'network' is not an igraph." = is.igraph(network),
    "'network' edges do not have a numeric 'draw' attribute." =
      is.numeric(E(network)$draw),
    "'network' does not have a character-vector states attribute." =
      is.character(network$states),
    "'V(network)$state' is not a character-vector." =
      is.character(V(network)$state),
    "'V(network)$state' are not all in 'network$states'." =
      all(V(network)$state %in% network$states),
    "'E(network)$draw' is not a probability." = all((0 <= E(network)$draw) & (E(network)$draw <= 1))
  )
  invisible(network)
}

#' @title Base SIR network for networks exercises
#'
#' @param N, a positive integer; the population size
#'
#' @details Note, this step actually performs all the relevant random number
#' draws, therefore to get matching results (e.g. to compare with other students)
#' the random number seed must be set ahead of calling this
#'
#' @return an igraph object, with N vertices, all connected by undirected edges
#'
#' @export
network_build <- function(
  N
) {
  # validate 'N'
  N |> assert_scalar() |> assert_natural()

  # make a network where everyone is connected
  network <- make_full_graph(N, directed = FALSE)

  # define model states. n.b.:
  # `network$NAME <- VALUE` is equivalent to `graph_attr(network, NAME) <- VALUE`
  network$states <- c("S", "I", "R")
  network$inf_states <- "I"

  # start out everyone as Susceptible
  V(network)$state <-    "S"
  # ... except set one Infectious individual.
  V(network)[1]$state <- "I"
  # @aside: Does it matter which individual is set to "I"?
  # @aside: We used `V(network)$state <- "S"` because its more obvious for beginners
  # what that does. However, why might e.g. `V(network)$state <- network$states[1]`
  # be preferable? Where else in the `MTM::network_...` function definitions
  # would that pattern also be preferred?

  # randomly draw a transmission value for each edge.
  E(network)$draw <- runif(ecount(network))
  # @aside: In the Reed-Frost model, how many times does a particular connection
  # between individuals need to be considered for transmission? What are some
  # model additions (e.g. different transitions) would change how many times a
  # connection is tested?

  # define edge attribute for showing transmission
  E(network)$active <- FALSE

  return(network)
}

#' @title transforms a network by percolation (selectively keeping / removing edges)
#'
#' @param network, an igraph object; the base network, it must have an edge attribute
#' 'draw'
#'
#' @return a new igraph object, with potentially fewer edges. it's 'p' attribute
#' will be == 1
#'
#' @export
network_percolate <- function(
  network, p
) {
  # validate network & p
  network |> assert_SIRgraph()
  p |> assert_scalar() |> assert_probability()

  # identify all edges where p < draw
  # i.e. going to *keep* all edges draw <= p
  remove_edges <- E(network)[ p < draw ]
  # return a new graph with removed edges & reset draw
  return(
    network |> delete_edges(remove_edges) |> set_edge_attr("draw", 0)
  )
}

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

#' @title Compute 1-step transition for Reed Frost Network Model
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
network_dReedFrost <- function(
  t, y, parms, ...
) {
  delta <- y
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
    # find all the transmission routes:
    #  1. find potential paths: `%->%` selects all edges from
    #     a left-hand-side vertex set (i.e. infectious)
    #     to a right-hand-side vertex set (i.e. susceptible)
    #  2. along possible transmission routes, some occur, based
    #     on draw value
    transmitting_paths <- E(delta)[
      infectious %->% susceptible
    ][
      draw <= parms$p
    ]

    # are there any transmitting paths?
    if (length(transmitting_paths)) {
      # infect all the individuals at the ends of those paths
      new_infections <- susceptible[.inc(transmitting_paths)]
      E(delta)[transmitting_paths]$active <- TRUE
      V(delta)[new_infections]$change <- "I"
    }
  }

  return(delta)
}

#' @title check if there any infectious individuals in network
#'
#' @param network an igraph, the population network
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

#' @title General Simulator for Network Populations.
#'
#' @description Simulates a discrete-time transition function on a network,
#' returning a series of network states
#'
#' @param y, an [igraph] network, the initial state of the population
#' 
#' @param times, a numeric vector, the times to report the state; cast
#' to `0:as.integer(max(times)). if `NULL` (the default) runs until extinct.
#' 
#' @param func, an R function, defined as `function(t, y, parms, ...)`, which
#' returns a list, first element a graph capturing state changes, and optional
#' second element the global values required at each point in time. The graph
#' must *exactly match* the vertices and edges in `y`.
#'
#' @param parms, parameters passed to `func`.
#' 
#' @param ..., other arguments passed to `func`.
#' 
#' @return a list of [igraph]s, where the list entries correspond to the population
#'   state at time t (e.g. `list[[1]]` is the initial network state after introduction)
#'
#' @details This function takes the Reed Frost model parameters (N, p) and implementation
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
network_solve <- function(
  y, times = NULL,
  func = network_dReedFrost,
  parms, ...
) {
  # initialize storage
  current_network <- y
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
#' @param network an igraph, representing a population OR a list of such igraphs
#'
#' @export
network_flatten <- function(network) {
  if (is.igraph(network)) {
    return(
      factor(V(network)$state, levels = network$states, ordered = TRUE) |>
      table() |> as.list() |> as.data.table()
    )
  } else { # assume it's a list of igraphs
    lapply(network, network_flatten) |> rbindlist(idcol = "t")
  }
}

#' @title sample a series of Reed-Frost SIR simulations
#'
#' @param n an integer; how many samples?
#' 
#' @inheritParams network_solve
#' 
#' @param setup_func a function to create new networks; must have the same signature as \code{\link{network_build}}
#' @param func a function to compute the next state; must have the same signature as \code{\link{network_dReedFrost}}
#' @param ref.seed a random seed reference value; each sample run seed is offset from this value
#' 
#' @return a \code{\link[data.table]{data.table}}, columns sample (integer, 1:n) & columns from \code{\link{network_flatten_run}}
#' 
#' @export
network_sample_ReedFrost <- function(
  n,
  parms,
  func = network_dReedFrost,
  setup_fun = network_build,
  ref.seed = 0
) {
  # TODO check parameters
  return(rbindlist(
    lapply(1:n, function(i) {
      set.seed(i + ref.seed);
      return(network_flatten(network_solve(setup_fun(N, p), update_fun)))
    }), idcol = "sample"
  ))
}

#' @title plot summary of a series of simulations
#'
#' @param s.dt a data.table, of the same structure as returned by \code{\link{network_sample_reed_frost}}
#'
#' @return a patchwork'd ggplot object
#'
#' @export
network_plot_histograms <- function(s.dt) {
  # assert: s.dt is ordered by t, and last entry by sample corresponds to end
  # assert: samples is 1:N
  ref.dt <- s.dt[, .SD[.N][,.(duration = t-1, final_size = R)], keyby=sample]
  samples <- ref.dt[, sample[.N]]

  # count data.tables
  heat.dt     <- ref.dt[, .N, keyby=.(duration, final_size)]
  durahist.dt <- ref.dt[, .N, keyby=.(duration)]
  sizehist.dt <- ref.dt[, .N, keyby=.(final_size)]

  # determine (ceiling rounded) probability of most likely outcome
  max.den <- ceiling(max(durahist.dt$N, sizehist.dt$N)/samples*10)/10

  # setup reference scales
  sx <- scale_x_continuous("Duration",   breaks = function(ls) seq(0, ls[2], by=4))
  sy <- scale_y_continuous("Final Size", breaks = function(ls) seq(0, ls[2], by=10))
  sdenx <- scale_x_continuous("Density", breaks = function(ls) seq(0, max.den, by=0.1))
  sdeny <- scale_y_continuous("Density", breaks = function(ls) seq(0, max.den, by=0.1))
  sdenfill <- scale_fill_distiller("Density", palette = "Reds", direction = 1)
  coords <- function(xmax = NA, ymax = NA) coord_cartesian(
    xlim = c(0, xmax), ylim = c(0, ymax), expand = FALSE
  )

  # convenience geom definition
  geom_dens <- function(fill = "grey60", stat = "identity", width = 1, ...) geom_bar(fill=fill, stat=stat, width=width, ...)
  
  # heat map of duration vs final size outcomes
  p.heat <- ggplot(heat.dt) + aes(duration, final_size, fill = N/samples) +
    geom_tile(alpha = 0.7) +
    sx + sy + sdenfill + coords() +
    theme_minimal() + theme(legend.position = c(.95, 0.05), legend.justification = c(1,0))

  # density plot of durations
  p.dur <- ggplot(durahist.dt) + aes(duration, N/samples) +
    geom_dens() +
    sx + sdeny + coords(ymax = max.den) +
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())

  # density plot of final sizes
  p.sz <- ggplot(sizehist.dt) + aes(N/samples, final_size) +
    geom_dens(orientation = "y") +
    sdenx + sy + coords(xmax = max.den) +
    theme_minimal() + theme(axis.title.y = element_blank(), axis.text.y = element_blank())

  # roll them all up
  # TODO replace plot_spacer with some text info?
  p.tot <- p.dur + plot_spacer() + p.heat + p.sz +
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

  return(p.tot)

}

#' @title plot the time series samples
#'
#' @inheritParams network_plot_histograms
#'
#' @export
network_plot_series <- function(s.dt, layer_order = c("S", "R", "I")) {
  # TODO also incorporate histograms here?
  # would be useful to show the final size + duration ones
  # but also prevalence peak / timing 
  if (!"sample" %in% colnames(s.dt)) {
    s.dt <- copy(s.dt)[, sample := 1 ]
  }
  alph <- s.dt[, 1/sqrt(length(unique(sample))) ]
  wide.dt <- melt(s.dt, id.vars = c("sample", "t"))
  return(ggplot(wide.dt) +
    aes(t, value, color = variable, group = interaction(variable, sample)) +
    lapply(layer_order, function(ly) geom_line(data = function(dt) dt[variable == ly], alpha = alph)) +
    scale_x_continuous(name = "Simulation time") +
    scale_y_continuous(name = NULL) +
    scale_color_manual(name = NULL, values = SIRcolors, guide = guide_legend(override.aes = list(alpha = 1))) +
    theme_minimal() +
    theme(legend.position = c(1, 0.5), legend.justification = c(1, 0.5))
  )
}

network_extract_active_directed <- function(net, el, vpos) {
  eact <- E(net)[active == TRUE]
  if (length(eact)) {
    src <- V(net)[.inc(eact)][state != "I"]
    tmp <- el[eact, , drop = FALSE]
    swp <- !(tmp[, 1, drop = FALSE] %in% src)
    tmp[swp, 1] <- tmp[swp, 2]
    tmp[swp, 2] <- el[eact, 1][swp]
    res <- cbind(vpos[tmp[,1],,drop=FALSE], vpos[tmp[, 2],,drop=FALSE])
    colnames(res) <- paste(colnames(res), rep(c("start", "end"), each = 2), sep = ".")
    as.data.table(res)[, eid := as.integer(eact) ]
  } else {
    data.table()
  }
}

#' @title create an animated plot of transmission on the population
#' 
#' @inheritParams network_flatten_run 
#' 
#' @return gganimate object
#' 
#' @export
network_animate_series <- function(network_run) {
  init <- network_run[[1]]
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
    c(network_run, network_run[length(network_run)]),
    network_extract_active_directed, el = epairs, vpos = pl
  ), idcol = "time")[, time := time - 1L ]
  
  v.states <- rbindlist(lapply(
    network_run, function(net) v.ref[, .(vid, vx, vy, state = V(net)$state) ]
  ), idcol = "time")

  # TODO hide edges as they become irrelevant to transmission?
  # TODO add labels for active infections, cumulative infections?
  
  return(ggplot() + geom_segment(
    aes(vx.start, vy.start, xend = vx.end, yend = vy.end, color = "inactive"),
    e.ref,
    size = 0.25, alpha = 0.5
  ) + 
    geom_segment(
      aes(vx.start, vy.start, xend = vx.end, yend = vy.end, color = "active", group = eid),
      e.active,
      size = 0.75, alpha = 1, arrow = arrow()
    ) +
    geom_point(
      aes(vx, vy, color = state, size = state, group = vid),
      v.states
    ) +
    transition_time(time) +
    scale_color_manual(guide = "none", values = c(SIRcolors, c(active="red", inactive="grey"))) +
    scale_size_manual(guide = "none", values = c(S=5, I=3, R=1)) +
    coord_equal() + theme_minimal() + theme(
    axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title = element_blank(), panel.grid = element_blank(),
    legend.position = "none"
  ) + labs(title = "Time: {frame_time}"))

}
