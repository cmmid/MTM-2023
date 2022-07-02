#' @import igraph data.table ggplot2 gganimate patchwork
NULL

#' @title network coloring scale
#'
#' @description a \code{\link[ggplot2]{scale_manual}} for coloring networks
#'
#' @param ... see \code{\link[ggplot2]{scale_manual}}, `scale_color_manual`
#'   arguments
#'
#' @seealso \code{\link{gg_scale_wrapper}}, \code{\link[ggplot2]{scale_manual}}
#'
#' @export
scale_color_network <- gg_scale_wrapper(
  scale_color_manual,
  guide = "none", values = c(SIRcolors, c(`TRUE`="red", `FALSE`="grey"))
)

#' @title vertex size scale
#'
#' @description a \code{\link[ggplot2]{scale_manual}} for sizing network vertices
#'
#' @param ... see \code{\link[ggplot2]{scale_manual}}, `scale_size_manual`
#'   arguments
#'
#' @seealso \code{\link{gg_scale_wrapper}}, \code{\link[ggplot2]{scale_manual}}
#'
#' @export
scale_size_vertex <- gg_scale_wrapper(
  scale_size_manual,
  guide = "none", values = c(S=5, I=3, R=1)
)

#' @title network plotting theme
#'
#' @param ... see \code{\link[ggplot2]{theme}} for all arguments
#'
#' @description a \code{\link[ggplot2]{theme}} wrapper function, with defaults
#'  for network plots
#'
#' @seealso gg_theme_wrapper, ggplot2::theme
#'
#' @export
network_theme <- gg_theme_wrapper(
  axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
  axis.title = element_blank(), panel.grid = element_blank(),
  legend.position = "none"
)

#' a ggplot2 geom for vertices
#'
#' @seealso gg_geom_wrapper, ggplot2::geom_point
#'
#' @export
geom_vertex <- gg_geom_wrapper(
  geom_point,
  mapping = aes(vx, vy, color = state, size = state, group = vid)
)

#' a ggplot2 geom for edges
#'
#' @seealso gg_geom_wrapper, ggplot2::geom_segment
#'
#' @export
geom_edge <- gg_geom_wrapper(
  geom_segment,
  mapping = aes(vx.start, vy.start, xend = vx.end, yend = vy.end, color = active)
)

#' provides a ggplot-based picture of vertices and edges
#'
#' @param e.ref data.frame, with columns v(x|y).(start|end) edge locations;
#'   un-directed, so order of start vs end not important
#' @param e.active data.frame, with columns v(x|y).(start|end) edge locations,
#'   corresponding to which edges are active; directed, so order matters
#'   end corresponds to point with arrow
#' @param v.states data.frame, with columns v(x|y) corresponding to vertex
#'   positions + state
#'
#' @export
network_ggplot <- function(
  e.ref, e.active, v.states
) return(
  ggplot() +
  geom_edge(
    data = e.ref,
    size = 0.25, alpha = 0.5
  ) +
  geom_edge(
    data = e.active,
    arrow = arrow(), size = 0.75
  ) +
  geom_vertex(data = v.states) +
  scale_color_network() +
  scale_size_vertex() +
  coord_equal() + theme_minimal() + network_theme()
)

#' extract table-formated data for plotting
#'
#' @param network an igraph object, with features as from `network_build`
#'
#' @return a list, with elements `v.ref` (the vertices reference) and
#' `e.ref` (the edges reference), both `data.table`s
#'
#' @export
network_to_ev_dts <- function(network) {
  v.ref <- as.data.table(
    network$layout
  )[, vid := 1L:.N ] |>
    setnames(old = c("V1", "V2"), new = c("vx", "vy"))
  e.ref <- as.data.table(
    as_edgelist(network)
  )[, eid := 1L:.N ][, active := FALSE ] |>
    setnames(old = c("V1", "V2"), new = c("start", "end"))
  e.ref[v.ref, c("vx.start", "vy.start") := .(vx, vy), on=.(start = vid)]
  e.ref[v.ref, c("vx.end"  , "vy.end"  ) := .(vx, vy), on=.(end = vid)]

  return(list(v.ref = v.ref, e.ref = e.ref))
}

#' @export
network_plot_one <- function(network) {
  ev_digest <- network_to_ev_dts(network)

  e.active <- with(
    ev_digest, network_extract_active_directed(network, e.ref)
  )
  v.states <- with(
    ev_digest, v.ref[, .(vid, vx, vy, state = V(network)$state) ]
  )

  return(network_ggplot(ev_digest$e.ref, e.active, v.states))

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
network_plot_series <- function(
  s.dt, layer_order = c("S", "R", "I")
) {
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

#' @export
network_extract_active_directed <- function(
  network, eref
) {
  eact <- E(network)[active == TRUE]
  if (length(eact)) {
    nonsrc <- V(network)[.inc(eact)][state != "I"]
    eactive <- eref[eid %in% as.integer(eact)]
    eactive[start %in% as.integer(nonsrc), c("start", "end") := .(end, start) ]
    return(eactive)
  } else {
    return(eref[0])
  }
}

#' @title create an animated plot of transmission on the population
#'
#' @param networks a list of igraphs
#'
#' @return gganimate object
#'
#' @seealso gganimate::anim_save
#'
#' @export
network_animate_series <- function(networks) {
  ev_digest <- network_to_ev_dts(networks[[1]])

  e.active <- rbindlist(lapply(
    c(networks, networks[length(networks)]),
    network_extract_active_directed, eref = ev_digest$e.ref
  ), idcol = "time")[, time := time - 1L ]

  v.states <- rbindlist(lapply(
    networks, function(net) ev_digest$v.ref[, .(vid, vx, vy, state = V(net)$state) ]
  ), idcol = "time")

  # TODO add labels for active infections, cumulative infections?

  return(
    network_ggplot(ev_digest$e.ref, e.active, v.states) +
    transition_time(time) +  labs(title = "Time: {frame_time}")
  )

}
