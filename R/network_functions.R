#' @import igraph data.table ggplot2 gganimate patchwork
NULL

check_SIRgraph <- function(network) {
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

check_RFparms <- function(parms) {
  stopifnot(
    "'parms' is not a list." = is.list(parms),
    "'parms' does not have members 'N' and 'p'." = c("N", "p") %in% names(parms)
  )
  parms$N |> check_scalar() |> check_natural()
  parms$p |> check_scalar() |> check_probability()
  invisible(parms)
}

#' @title Base SIR network for networks exercises
#'
#' @param parms, a `list(N=integer, p=probability)`; the Reed-Frost model parameters.
#'
#' @details Note, this step actually performs all the relevant random number
#' draws, therefore to get matching results (e.g. to compare with other students)
#' the random number seed must be set ahead of calling this
#'
#' @section Asides:
#' Note that in the function body, the first vertex is set to "I".
#' Does it matter which individual is set to "I"? Why or why not?
#' 
#' The function uses `V(network)$state <- "S"` because its
#' more obvious for beginners what that does. However, why might
#' e.g. `V(network)$state <- network$states[1]` be preferable? Where else
#' in the `MTM::network_...` function definitions would that pattern also
#' be preferred?
#'
#' In the Reed-Frost model, how many times does a particular connection
#' between individuals need to be considered for transmission? What are some
#' model additions (e.g. different transitions) would change how many times a
#' connection is tested?
#'
#' @return an igraph object, with `parms$N` vertices, all connected by
#' undirected edges. Initialized with one *I*nfectious vertex, and edges
#' each assigned a random number on (0, 1).
#'
#' @examples 
#' require(MTM)
#' require(igraph) # for plotting utilities
#' quickplot <- function(net) plot(net,
#'   vertex.size = 5,
#'   # set color according to state
#'   vertex.color = SIRcolors[V(net)$state],
#'   vertex.frame.color = NA, vertex.label = NA,
#'   # visually emphasize connections more likely to transmit
#'   edge.width = (1-E(net)$draw)^3,
#'   edge.color = rgb(red = 0.6, 0, 0, alpha = (1-E(net)$draw)^3)
#' )
#' # repeat these next steps a few times, w/ different `N`
#' # to see how `network_build()` works.
#' sirpop <- network_build(list(N=30, p=0.2))
#' quickplot(sirpop)
#' 
#' @export 
network_build <- function(
  parms
) {
  # validate parms
  parms |> check_RFparms()
  # make a network where everyone is connected
  network <- make_full_graph(parms$N, directed = FALSE)
  # define model states. as graph attributes. see also: `?graph_attr`
  network$states <- c("S", "I", "R")
  network$inf_states <- "I"
  # start out everyone as *S*usceptible
  V(network)$state <-    "S"
  # ... except set one *I*nfectious individual.
  V(network)[1]$state <- "I"
  # draw a random transmission value for each edge.
  E(network)$draw <- runif(ecount(network))
  # define edge attribute for showing transmission
  E(network)$active <- FALSE
  return(network)
}

#' @title Transform a network by percolation
#'
#' @inheritParams network_build
#' 
#' @param network, an igraph object, such as produced by [network_build];
#' for when you want to repeatedly percolate the same base network with
#' different probabilities.
#'
#' @return a new igraph object, with potentially fewer edges. it's 'E()$draw' attribute
#' will be == 0 (i.e., always transmit)
#'
#' @examples
#' require(MTM)
#' require(igraph) # for plotting utilities
#' parms <- list(N=30, p=0.2)
#' sirpop.base <- network_build(parms)
#' layout.base <- layout_nicely(sirpop.base)
#' 
#' quickplot <- function(net) plot(net,
#'   vertex.size = 5,
#'   # set color according to state
#'   vertex.color = SIRcolors[V(net)$state],
#'   vertex.frame.color = NA, vertex.label = NA,
#'   # visually emphasize connections more likely to transmit
#'   edge.width = (1-E(net)$draw)^3,
#'   edge.color = rgb(red = 0.6, 0, 0, alpha = (1-E(net)$draw)^3),
#'   layout = layout.base
#' )
#' 
#' # repeat these next steps a few times, w/ different `p` to see
#' # to see how `network_percolate()` works.
#' sirpop.perc <- network_percolate(within(parms, p <- 0.05), sirpop.base)
#' quickplot(sirpop.perc)
#' 
#' @export
network_percolate <- function(
  parms, network = network_build(parms)
) {
  # validate parms and network
  parms |> check_RFparms()
  network |> check_SIRgraph()
  # identify all edges where p < draw
  # i.e. going to *keep* all edges draw <= p
  remove_edges <- E(network)[ parms$p < draw ]
  # return a new graph with removed edges & reset draw
  return(
    network |> delete_edges(remove_edges) |> set_edge_attr(name = "draw", value = 0)
  )
}

#' @title compute a new network from initial states & changes
#'
#' @param y, an `igraph`, the initial network state
#' @param dy, an `igraph`, the vertex states that change & edges that were
#'   activated leading to those changes
#'
#' @return a new igraph, representing the state changes applied
#'
#' @details Note: this function is only exported so that students
#' can see the internals easily. It is intended to be used
#' internally to other functions in the package, where the
#' arguments are already assured, so it does not include
#' `check_...` steps, which would be too expensive to repeatedly
#' calculate.
#'
#' @examples
#' population <- network_build_basic(N = 30, p = 0.1)
#' # TODO
#'
#' @export
network_update <- function(
  y, dy
) {
  # get the subset of vertices with changed state
  changedv <- V(dy)[!is.na(change)]
  # if there are any updates ...
  if (length(changedv)) {
    # apply them to original network
    V(y)[changedv]$state <- changedv$change
    E(y)$active <- E(dy)$active
  }
  return(y)
}

#' @title Compute 1-step transition for Reed-Frost Network Model
#'
#' @param t, the time step; ignored in the Reed-Frost model
#'
#' @inheritParams network_solve
#'
#' @return an `igraph` representing the transitions that occur and edges involved
#'
#' @seealso network_solve
#' 
#' @examples
#' require(MTM)
#' require(igraph) # for plotting utilities
#' parms <- list(N=30, p=0.1)
#' set.seed(8675309)
#' sirpop <- network_build(parms)
#' layout.base <- layout_nicely(sirpop)
#' onestep <- network_dReedFrost(0, sirpop, parms)
#' 
#' quickplot <- function(net, vcols, ecols) plot(net,
#'   vertex.size = 5,
#'   # set color according to state
#'   vertex.color = vcols,
#'   vertex.frame.color = NA, vertex.label = NA,
#'   # visually emphasize connections more likely to transmit
#'   edge.width = (1-E(net)$draw)^3,
#'   edge.color = ecols,
#'   layout = layout.base
#' )
#' # the initial state
#' quickplot(
#'   sirpop,
#'   vcols = SIRcolors[V(sirpop)$state],
#'   ecols = rgb(red = 0.6, 0, 0, alpha = (1-E(sirpop)$draw)^3)
#' )
#' # the changes
#' quickplot(
#'   onestep,
#'   vcols = fifelse(is.na(V(onestep)$change), "grey", SIRcolors[V(onestep)$change]),
#'   ecols = fifelse(E(onestep)$active, "red", "lightgrey")
#' )
#'
#' @export
network_dReedFrost <- function(
  t, y, parms, ...
) {
  # copy y to become dy
  dy <- y
  # the individuals that are infectious or susceptible,
  # as igraph vertex sets
  infectious <- V(dy)[state == "I"]
  susceptible <- V(dy)[state == "S"]
  # all infectious individuals will recover
  V(dy)[infectious]$change <- "R"
  # whatever happened previously now over
  E(dy)$active <- FALSE

  # if there are infectious & susceptible individuals
  if (length(susceptible) & length(infectious)) {
    # find all the transmission routes:
    #  1. find potential paths: `%->%` selects all edges from
    #     a left-hand-side vertex set (i.e. infectious)
    #     to a right-hand-side vertex set (i.e. susceptible)
    #  2. along possible transmission routes, some occur, based
    #     on draw value
    transmitting_paths <- E(dy)[
      infectious %->% susceptible
    ][
      draw <= parms$p
    ]

    # are there any transmitting paths?
    if (length(transmitting_paths)) {
      # infect all the individuals at the ends of those paths
      new_infections <- susceptible[.inc(transmitting_paths)]
      E(dy)[transmitting_paths]$active <- TRUE
      V(dy)[new_infections]$change <- "I"
    }
  }

  return(dy)
}

#' @title Check for Infectious individuals in network
#'
#' @inheritParams network_solve
#'
#' @return `TRUE` or `FALSE`
#'
#' @examples
#' population <- network_build_basic(N = 30, p = 0.1)
#' # TODO
#'
#' @export
network_is_infectious <- function(
  y
) {
  return(length(V(y)[state %in% y$inf_states]) > 0)
}

#' @title General Simulator for Network Populations.
#'
#' @description Simulates a discrete-time transition function on a network,
#' returning a series of network states
#'
#' @param y, an [igraph] network, the initial state of the population
#' 
#' @param times, a numeric vector, the times to report the state; cast
#' to `0:as.integer(max(times))`. If `NULL` (the default) runs until extinct.
#' 
#' @param func, an R function, defined as `function(t, y, parms, ...)`, which
#' returns a graph capturing state changes. The graph must have the same
#' vertices and edges as in `y`, but can have different attributes.
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
#' require(MTM)
#' require(igraph) # for plotting utilities
#' # guarantee a network with some transmission
#' set.seed(42)
#' parms <- list(N = 30, p = 0.1)
#' ys <- network_solve(
#'   y = network_build(parms),
#'   parms = parms
#' )
#'
#' layout.base <- layout_nicely(ys[[1]])
#' quickplot <- function(net) plot(net,
#'   vertex.size = 5,
#'   # set color according to state
#'   vertex.color = SIRcolors[V(net)$state],
#'   vertex.frame.color = NA, vertex.label = NA,
#'   # visually emphasize connections more likely to transmit
#'   edge.width = (1-E(net)$draw)^3,
#'   edge.color = fifelse(E(net)$active, "red", "lightgrey"),
#'   layout = layout.base
#' )
#' quickplot(ys[[1]])
#' quickplot(ys[[2]])
#' quickplot(ys[[3]])
#' quickplot(ys[[4]])
#'
#' @export
network_solve <- function(
  y, times = NULL,
  func = network_dReedFrost,
  parms, ...
) {
  # initialize storage
  yt  <- list(y)
  t <- 0L
  tmax <- ifelse(
    is.null(times),
    .Machine$integer.max,
    max(as.integer(times))
  )
  while(network_is_infectious(y) && (t < tmax)) {
    dy <- func(t, y, parms, ...)
    y <- network_update(y, dy)
    yt <- c(list(y), yt)
    t <- t + 1L
  }
  return(rev(yt))
}

#' @title Aggregate Network into States
#' 
#' @param y an `igraph`, representing a population OR a list of such `igraph`s
#'
#' @export
network_flatten <- function(y) {
  if (is.igraph(y)) {
    return(
      factor(V(y)$state, levels = y$states, ordered = TRUE) |>
      table() |> as.list() |> as.data.table()
    )
  } else { # assume it's a list of igraphs
    lapply(y, network_flatten) |> rbindlist(idcol = "t")
  }
}

#' @title sample a series of Reed-Frost SIR simulations
#'
#' @param n an integer; how many samples?
#' 
#' @inheritParams network_solve
#' 
#' @param setup_func a function to create new networks; must have the same signature as [network_build]()
#' 
#' @param ref.seed a random seed reference value; each sample run seed is offset from this value
#' 
#' @return a [data.table::data.table], a sample column (integer, 1:`n`) &
#' columns from [network_flatten]
#' 
#' @export
network_sample_ReedFrost <- function(
  n,
  parms,
  func = network_dReedFrost,
  setup_fun = network_build,
  ref.seed = 0
) {
  n |> check_scalar() |> check_natural()
  parms |> check_RFparms()
  # for each sample ...
  1L:n |> lapply(function(i) {
    # reset random number seed
    set.seed(i + ref.seed)
    # make a new population
    setup_fun(parms) |>
    # solve it according to desired func
    network_solve(func = func, parms = parms) |>
    # reduce it to just states counts by time
    network_flatten()
  }) |> rbindlist(idcol = "sample")
}

#' @title Summarize Network Runs
#' 
#' @description Calculates final size and duration of
#' simulated epidemics on networks.
#' 
#' @param dt, an object coercable by [data.table::as.data.table()].
#' Expected to have columns `sample`,`t`, and `R`.
#' See [network_sample()] return value
#' 
#' @export
network_summarize <- function(
  dt
) {
  as.data.table(dt)[,
    .SD[.N][, .(duration = t-1, finalsize = R) ],
    keyby = sample
  ]
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
#' @inheritParams network_flatten
#' 
#' @return gganimate object
#' 
#' @export
network_animate <- function(ys) {
  init <- ys[[1]]
  pl <- layout_(init, with_fr(coords = layout_as_star(init)), normalize())
  colnames(pl) <- c("vx", "vy")
  epairs <- as_edgelist(init)
  starts <- pl[epairs[,1],]
  colnames(starts) <- paste0(colnames(starts), ".start")
  ends <- pl[epairs[,2],]
  colnames(ends) <- paste0(colnames(ends), ".end")
  e.ref <- as.data.table(cbind(starts, ends))[, eid := 1L:.N ]
  v.ref <- as.data.table(pl)[, vid := 1L:.N ]
  
  e.active <- c(ys, ys[length(ys)]) |>
    lapply(
      network_extract_active_directed,
      el = epairs, vpos = pl
    ) |> rbindlist(idcol = "time")
  e.active[, time := time - 1L ]
  
  v.states <- ys |> lapply(
    function(net) v.ref[, .(vid, vx, vy, state = V(net)$state) ]
  ) |> rbindlist(idcol = "time")

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
    scale_color_compartment(
      guide = "none",
      values = c(SIRcolors, c(active="red", inactive="grey"))
    ) +
    scale_size_manual(guide = "none", values = c(S=5, I=3, R=1)) +
    coord_equal() + theme_minimal() + theme(
    axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title = element_blank(), panel.grid = element_blank(),
    legend.position = "none"
  ) + labs(title = "Time: {frame_time}"))

}
