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
    "'E(network)$draw' is not a probability." = all(
        (0 <= E(network)$draw) & (E(network)$draw <= 1)
      )
  )
  invisible(network)
}

check_RFparms <- function(parms) {
  stopifnot(
    "'parms' is not a list." = is.list(parms),
    "'parms' does not have members 'N' and 'p'." = c("N", "p") %in% names(parms)
  )
  check_natural(check_scalar(parms$N))
  check_probability(check_scalar(parms$p))
  invisible(parms)
}

#' @title Base SIR network for networks exercises
#'
#' @param parms, a `list(N = integer, p = probability)`; the Reed-Frost model
#' parameters.
#'
#' @details Note, this step actually performs all the relevant random number
#' draws, therefore to get matching results (e.g. to compare to other students)
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
#' model additions (_e.g._ different transitions) that would change how many
#' times a connection is tested?
#'
#' @return an [igraph], fully connected, with undirected edges, and `parms$N`
#' vertices. Initialized with one *I*nfectious vertex, and [igraph::edge_attr()]
#' `draw`, a random deviate on (0, 1).
#'
#' @examples
#' require(MTM)
#'
#' # repeat these next steps a few times, w/ different `N`
#' # to see how `network_build()` works.
#' sirpop <- network_build(list(N=30, p=0.2))
#' network_quickplot(sirpop, edgeargs = list(), vertexargs = list())
#'
#' @export
network_build <- function(
  parms
) {
  # validate parms
  check_RFparms(parms)
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
  E(network)$state <- "inactive"

  seedstore <- .Random.seed
  set.seed(parms$N) # fix rng for layout => all size N networks have same layout
  network <- add_layout_(network, with_fr(
    coords = layout_randomly(network)), normalize()
  )
  .Random.seed <- seedstore

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
#' @return a new igraph object, with potentially fewer edges. it's `E()$draw`
#' attribute will be == 0 (i.e., always transmit)
#'
#' @examples
#' require(MTM)
#' parms <- list(N=30, p=0.2)
#'
#' # repeat these next steps a few times, w/ different `p` to see
#' # to see how `network_percolate()` works.
#' sirpop <- network_percolate(within(parms, p <- 0.05))
#' network_quickplot(sirpop, edgeargs = list(), vertexargs = list())
#'
#' @export
network_percolate <- function(
  parms, network = network_build(parms)
) {
  # validate parms and network
  check_RFparms(parms)
  check_SIRgraph(network)
  # identify all edges where p < draw
  # i.e. going to *keep* all edges draw <= p
  remove_edges <- E(network)[parms$p < draw]
  # return a new graph with removed edges & reset draw
  return(
    set_edge_attr(delete_edges(network, remove_edges), name = "draw", value = 0)
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
    E(y)$state <- E(dy)$state
  }
  return(y)
}

#' @title Compute 1-step transition for Reed-Frost Network Model
#'
#' @param t, the time step; ignored in the Reed-Frost model
#'
#' @inheritParams network_solve
#'
#' @return an `igraph` representing the transitions that occur and edges
#' involved
#'
#' @seealso network_solve
#'
#' @examples
#' require(MTM)
#' parms <- list(N=30, p=0.1)
#' set.seed(8675309)
#' sirpop <- network_build(parms)
#' onestep <- network_dReedFrost(0, sirpop, parms)
#'
#' # the initial state
#' network_quickplot(sirpop)
#'
#' # the changes
#' network_quickplot(onestep)
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
  E(dy)$state <- "inactive"

  # if there are infectious & susceptible individuals
  if (length(susceptible) && length(infectious)) {
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
      E(dy)[transmitting_paths]$state <- "active"
      V(dy)[new_infections]$change <- "I"
    }
  }

  return(dy)
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
#' @return a list of [igraph]s, where the list entries correspond to the
#'   population state at time t (e.g. `list[[1]]` is the initial network state
#'   after introduction)
#'
#' @details This function takes the Reed Frost model parameters (N, p) and
#' implementation of the model (set by `setupfun`, `deltafun`). Using those, it
#' performs the model iteration loop, and returns the series networks
#'
#' @examples
#' require(MTM)
#' # guarantee a network with some transmission
#' set.seed(42)
#' parms <- list(N = 30, p = 0.1)
#' ys <- network_solve(
#'   y = network_build(parms),
#'   parms = parms
#' )
#'
#' network_quickplot(ys[[1]])
#' network_quickplot(ys[[2]])
#' network_quickplot(ys[[3]])
#' network_quickplot(ys[[4]])
#'
#' @export
network_solve <- function(
  y = network_build(parms),
  times = NULL,
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
  while (
    any(V(y)$state %in% y$inf_states) && (t < tmax)
  ) {
    dy <- func(t, y, parms, ...)
    y <- network_update(y, dy)
    yt[[length(yt) + 1]] <- y
    t <- t + 1L
  }
  return(yt)
}

#' @title Aggregate Network into States
#'
#' @param y an `igraph`, representing a population OR a list of such `igraph`s
#'
#' @export
network_flatten <- function(y, one = is.igraph(y)) {
  if (one) {
    return(as.data.table(as.list(table(
      factor(V(y)$state, levels = y$states, ordered = TRUE)
    ))))
  } else { # assume it's a list of igraphs
     return(
       rbindlist(lapply(y, network_flatten, one = TRUE), idcol = "t")
     )
  }
}

#' @title sample a series of Reed-Frost SIR simulations
#'
#' @param n an integer; how many samples?
#'
#' @inheritParams network_solve
#'
#' @param setup_func a function to create new networks; must have the same
#' signature as [network_build]()
#'
#' @param ref_seed a random seed reference value; each sample run seed is offset
#' from this value
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
  ref_seed = 0
) {
  check_natural(check_scalar(n))
  check_RFparms(parms)
  # for each sample ...
  rbindlist(lapply(seq_len(n), function(i) {
    # reset random number seed
    set.seed(i + ref_seed)
    # make a new population: setup_fun(parms)
    # solve it according to desired func
    # reduce it to just states counts by time: network_flatten()
    network_flatten(network_solve(setup_fun(parms), func = func, parms = parms))
  }), idcol = "sample")
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
    .SD[.N, .(duration = t - 1, finalsize = R)],
    keyby = sample
  ]
}
