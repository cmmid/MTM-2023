#' @import igraph data.table ggplot2 gganimate patchwork
NULL

#' @title Convert [igraph] to [data.table]
#'
#' @param ig the [igraph]
#'
#' @param keep.rownames logical; not yet supported. TODO: have this cause
#' preservation of vertex and edge named IDs (if present)
#'
#' @return a [data.table], with columns `V2`, `V1` (vertex ids), `eid` (edge ids),
#' layout data (`x1`, `y1`, `x2`, `y2`), and then optionally edge attributes (
#' various, corresponding to the [igraph::edge_attr()] of `ig`) and vertex
#' attributes (various, corresponding to the [igraph::vertex_attr()] of `ig`,
#' prepended with `V1` and `V2` to identify associated vertex).
#'
#' @export as.data.table.igraph
#' @exportS3Method
as.data.table.igraph <- function(ig, keep.rownames, ...) {
  # extract edge properties
  el <- ig |> as_edgelist() |> as.data.table()
  el[, eid := 1:.N ]
  if (ig |> edge_attr_names() |> length()) {
    eprops <- ig |> edge_attr() |> as.data.table()
    eprops[, eid := 1:.N ]
    base <- merge(eprops, el, by="eid")
  } else {
    base <- el
  }

  if (is.null(ig$layout)) ig$layout <- layout_nicely(ig)

  vl <- ig$layout |> as.data.table() |> setnames(c("V1", "V2"), c("x", "y"))
  vl[, vid := 1:.N ]
  # model of resulting data: edges, edge properties, vertex ids, vertex properties
  base[vl, on=.(V1=vid), c("x1", "y1") := .(x, y)]
  base[vl, on=.(V2=vid), c("x2", "y2") := .(x, y)]

  if (vl[!(vid %in% base[, union(V1, V2)]), .N]) {
    base <- rbind(
      base,
      vl[!(vid %in% base[, union(V1, V2)]), .(V1 = vid, x1 = x, y1 = y)],
      fill = TRUE
    )
  }

  # extract vertex properties
  vpropnames <- ig |> vertex_attr_names()
  if (vpropnames |> length()) {
    vprops <- ig |> vertex_attr() |> as.data.table()
    vprops[, vid := 1:.N ]
    setnames(vprops, vpropnames, paste0("V1.", vpropnames))
    vpropnames <- paste0("V1.", vpropnames)
    setnames(vprops, "vid", "V1")
    extended <- merge(base, vprops, by="V1")
    setnames(vprops, vpropnames, gsub("V1", "V2", vpropnames))
    vpropnames <- gsub("V1", "V2", vpropnames)
    setnames(vprops, "V1", "V2")
    extended <- merge(extended, vprops, by="V2", all.x = TRUE)
  } else {
    extended <- base
  }

  for (gn in setdiff(graph_attr_names(ig), c("layout", "sorted"))) attr(extended, gn) <- graph_attr(ig, gn)

  setkey(extended, eid, V1, V2) |> setcolorder()
}

#' @title GGplot for [igraph]
#'
#' @description a [ggplot2::ggplot()] specialization for [igraph] objects
#'
#' @param data an [igraph] object
#'
#' @inheritDotParams ggplot2::ggplot
#'
#' @details Applies [as.data.table.igraph()] to `data` and then passes it into
#' [ggplot2::ggplot()]
#'
#' @export ggplot.igraph
#' @exportS3Method
ggplot.igraph <- function(data, mapping = aes(), ..., environment = parent.frame()) {
  return(ggplot2::ggplot(data |> as.data.table.igraph(), mapping, ..., environment))
}

#' @title Get Vertex Data
#'
#' @description Extract vertex data from [data.table]s made from [igraph]s
#'
#' @param dt [data.table] object from [as.data.table.igraph()]
#'
#' @details Intended for use with [geom_vertex()]. Extracts the unique vertices,
#' their positions, and any attribute data.
#'
#' @export
network_vertex_data <- function(dt) {
  if (!is(dt, "data.table")) dt <- as.data.table(dt)
  v1 <- unique(dt[,.SD,.SDcols = patterns("^V1|x1|y1|time")])
  setnames(v1, c("V1", "x1", "y1"), c("vid", "x", "y"))
  setnames(v1, names(v1), gsub("V1\\.","", names(v1)))
  v2 <- unique(dt[,.SD,.SDcols = patterns("^V2|x2|y2|time")])[!(V2 %in% v1$vid)]
  setnames(v2, c("V2", "x2", "y2"), c("vid", "x", "y"))
  setnames(v2, names(v2), gsub("V2\\.","", names(v2)))
  return(rbind(v1, v2) |> subset(!is.na(vid)) |> setkey(vid))
}

#' @title Get Edge Data
#'
#' @description Extract edge data from [data.table]s made from [igraph]s
#'
#' @param dt [data.table] object from [as.data.table.igraph()]
#'
#' @details Intended for use with [geom_edge()]. Extracts the unique edges,
#' their ends, and any attribute data.
#'
#' @export
network_edge_data <- function(dt) {
  if (!is(dt, "data.table")) dt <- as.data.table(dt)
  return(dt[, .SD, .SDcols = !patterns("^V[12]")] |> unique() |>
    subset(!is.na(eid)) |> setkey(eid))
}

#' @title Network Color Scale
#'
#' @description a [ggplot2::scale_color_manual] for coloring networks
#'
#' @inheritParams ggplot2::scale_color_manual
#'
#' @export
scale_color_network <- rejig(
  ggplot2::scale_color_manual,
  guide = "none", values = c(SIRcolors, c(active="red", inactive="grey"))
)

#' @title Vertex Size Scale
#'
#' @inheritParams ggplot2::scale_size_manual
#'
#' @export
scale_size_vertex <- rejig(
  scale_size_manual,
  guide = "none", values = c(S=5, I=3, R=1)
)

#' @title network plotting theme
#'
#' @inheritParams ggplot2::theme_minimal
#'
#' @description a [ggplot2::theme], with defaults for network plots
#'
#' @export
theme_network <- function(
  base_size = 11, base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
) {
  theme_minimal(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) + theme(
    axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title = element_blank(), panel.grid = element_blank()
  )
}

#' @title GGPlot-like Vertex Geom
#'
#' @inheritParams ggplot2::geom_point
#'
#' @export
geom_vertex <- rejig(
  geom_point,
  mapping = aes(x = x, y = y, color = state, group = vid),
  data = network_vertex_data
)

#' @title GGPlot-like Vertex Geom
#'
#' @inheritParams ggplot2::geom_segment
#'
#' @export
geom_edge <- rejig(
  geom_segment,
  mapping = aes(x = x1, y = y1, xend = x2, yend = y2, color = state, group = eid),
  data = network_edge_data
)


#' @title Demo Network Plotter
#'
#' @description Provides a quick plotter with most elements constrained
#'
#' @param ig an [igraph]
#'
#' @param edgeargs a list of arguments to [geom_edge()]
#'
#' @param vertexargs a list of arguments to [geom_vertex()]
#'
#' @inheritParams ggplot2::scale_color_manual
#'
#' @return a [ggplot2::ggplot]
#'
#' @export
network_quickplot <- function(
  ig, values, labels,
  edgeargs = if (simple) list(color = "grey90") else list(),
  vertexargs = if (simple) list(color = "black") else list(),
  simple = FALSE
) {
  return(eval(substitute(
    ggplot(ig) + do.call(geom_edge, edgeargs) +
    do.call(geom_vertex, vertexargs) +
    coord_equal() +
    scale_color_network(
      values = values, labels = labels
    ) + theme_network()
  )))
}

#' @export
network_plot_RF <- function(ig) {
  return(
    ggplot(ig) + geom_edge() +
      geom_edge(data = network_active_edges, arrow = arrow()) +
      geom_vertex() +
      coord_equal() +
      scale_color_network(
        values = c(SIRcolors, active = "red", inactive = "grey")
      ) + theme_network()
  )
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
    scale_x_simtime() +
    scale_y_continuous(name = NULL) +
    scale_color_manual(name = NULL, values = SIRcolors, guide = guide_legend(override.aes = list(alpha = 1))) +
    theme_minimal() +
    theme(legend.position = c(1, 0.5), legend.justification = c(1, 0.5))
  )
}

# TODO turn this into specialized version of network_edge_data
# function(dt) {
#   return(dt[, .SD, .SDcols = !patterns("^V[12]")] |> unique() |>
#            subset(!is.na(eid)) |> setkey(eid))
# }

#' @title Extract Active Edges
#'
#' @param dt a [data.table] resulting from [as.data.table.igraph()],
#' applied to an [igraph] with both vertex and edge attributes `state`.
#'
#'
#' @export
network_active_edges <- function(
  dt
) {
  # extract the active edges ...
  activesub <- dt[state == "active"]

  if (activesub[, .N]) {
    inf_states <- attr(activesub, "inf_states")
    # if there are any, return them with V1 and V2 flipped where V2 state is infectious
    return(rbind(
      activesub[V2.state %in% inf_states],
      copy(activesub[V1.state %in% inf_states])[, c("x1", "y1", "x2", "y2") := .(x2, y2, x1, y1)]
    ) |> network_edge_data())
  } else {
    return(activesub)
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
network_animate <- function(networks) {
  merge_attrs <- setdiff(graph_attr_names(networks[[1]]), c("layout", "sorted"))
  dts <- c(networks, networks[length(networks)]) |> lapply(as.data.table.igraph) |> rbindlist(idcol = "time")

  for (gn in merge_attrs) attr(dts, gn) <- graph_attr(networks[[1]], gn)

  # TODO add labels for active infections, cumulative infections?
  return(
    (dts |> network_plot_RF()) +
      transition_time(time) + labs(title = "Time: {frame_time}")
  )

}
