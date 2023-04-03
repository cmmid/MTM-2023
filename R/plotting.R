
#' @inheritParams ggplot2::scale_x_continuous
#'
#' @rdname plotting
#'
#' @export
scale_x_density <- rejig(
  scale_x_continuous, name = "Density"
)

#' @inheritParams ggplot2::scale_y_continuous
#'
#' @rdname plotting
#'
#' @export
scale_y_density <- rejig(
  scale_y_continuous, name = "Density"
)

#' @inheritParams ggplot2::scale_fill_distiller
#'
#' @rdname plotting
#'
#' @export
scale_fill_density <- rejig(
  scale_fill_distiller, name = "Density",
  palette = "Reds", direction = 1
)

#' @inheritParams ggplot2::scale_x_continuous
#'
#' @rdname plotting
#'
#' @export
scale_x_duration <- rejig(
  scale_x_continuous, name = "Duration"
)

#' @inheritParams ggplot2::scale_x_continuous
#'
#' @rdname plotting
#'
#' @export
scale_x_simtime <- rejig(
  scale_x_continuous, name = expression(t)
)

#' @inheritParams ggplot2::scale_color_manual
#'
#' @rdname plotting
#'
#' @export
scale_color_compartment <- rejig(
 scale_color_manual,
 name = NULL, values = SIRcolors,
 guide = guide_legend(override.aes = list(alpha = 1))
)

#' @inheritParams ggplot2::geom_bar
#'
#' @rdname plotting
#'
#' @export
geom_edf <- rejig(
  geom_bar,
  fill = "grey60", stat = "identity", width = 1
)

#' @inheritDotParams ggplot2::coord_cartesian
#'
#' @param xmax alternate parameterisation for xlim
#'
#' @param ymax alternate parameterisation for ylim
#'
#' @export
coord_max <- function(
  xmax = NA, ymax = NA,
  xlim = c(0, xmax), ylim = c(0, ymax),
  expand = FALSE, ...
) coord_cartesian(
  xlim = xlim, ylim = ylim,
  expand = expand, ...
)

#' @title defines reference colors used in plots throughout MTM course
#'
#' @export
SIRcolors <- c(S = "dodgerblue", I = "firebrick", R = "forestgreen")

#' @title Multi-dimensional density plot
#'
#' @param dt a [data.table], with a `sample` column and at least other two columns
#' of properties to visualise
#'
#' @return a [patchwork]'d [ggplot2] object
#'
#' @export
plot_2D_density <- function(
    dt,
    margins = setdiff(colnames(s.dt), "sample")[1:2],
    scales = mget(sprintf("scale_%s_%s", c("x", "y"), margins))
) {

  samples <- dt[, length(sample)]

  mkeys <- as.list(margins)
  names(mkeys) <- c("mx", "my")

  # count data.tables
  heat.dt     <- dt[, .N, keyby = margins]
  histmx.dt <- dt[, .N, keyby = margins["mx"] ]
  histmy.dt <- dt[, .N, keyby = margins["my"] ]

  # determine (ceiling rounded) probability of most likely outcome
  max.den <- ceiling(max(histmx.dt$N, histmx.dt$N)/samples*10)/10

  # setup reference scales
  sx <- scales[[1]](breaks = function(ls) seq(0, ls[2], by=4))
  sy <- scales[[2]](breaks = function(ls) seq(0, ls[2], by=10))
  breakfun <- function(ls) seq(0, max.den, by=0.1)
  sdenx <- scale_x_density(breaks = function(ls) seq(0, max.den, by=0.1))
  sdeny <- scale_y_density(breaks = function(ls) seq(0, max.den, by=0.1))
  sdenfill <- scale_fill_distiller("Density", palette = "Reds", direction = 1)

  # heat map of duration vs final size outcomes
  p.heat <- ggplot(heat.dt) +
    aes(mx, my, fill = N/samples) +
    geom_tile(alpha = 0.7) +
    sx + sy + scale_fill_density() +
    coord_max() +
    theme_minimal() +
    theme(
      legend.position = c(.95, 0.05), legend.justification = c(1,0)
    )

  # density plot of margin 1
  p.mx <- ggplot(histmx.dt) + aes(mx, N/samples) +
    geom_dens() +
    sx + sdeny + coords(ymax = max.den) +
    theme_minimal() + theme(
      axis.title.x = element_blank(), axis.text.x = element_blank()
    )

  # density plot of margin 2
  p.my <- ggplot(histmy.dt) + aes(N/samples, my) +
    geom_dens(orientation = "y") +
    sdenx + sy + coords(xmax = max.den) +
    theme_minimal() + theme(
      axis.title.y = element_blank(), axis.text.y = element_blank()
    )

  # roll them all up
  # TODO replace plot_spacer with some text info?
  p.tot <- p.dur + plot_spacer() + p.heat + p.sz +
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

  return(p.tot)

}

#' @title Generic Compartment Plot
#'
#' @description Provides a consistent plot of compartment model results across all sessions.
#'
#' @param dt a [data.table::data.table()] with a column `t` (for simulation time), optionally
#' a `sample` column (for stochastic replicates), and overall in wide format (each other column
#' referring to compartment) or long format (having `variable` and `value` columns, with `variable`
#' indicating compartment and `value` the value of that compartment at that time and optionally sample)
#'
#' @param compartment.order a character vector, indicating the layer order for the compartments. N.B.
#' the "first" layer is the first layer drawn (i.e. the bottom-most) and "last" is the last drawn
#' (i.e. the top-most, typically most visible).
#'
#' @details This function is a convenient wrapper to create a [ggplot2::ggplot()] object from typical
#' series data output from example simulations.
#'
#' @export
plot_series <- function(
  dt,
  compartment.order = c("S", "R", "I")
) {
  if (!"sample" %in% colnames(dt)) {
    dt <- as.data.table(dt)[, sample := 1 ]
  }
  alph <- dt[, 1/sqrt(length(unique(sample))) ]
  long.dt <- melt(
    dt, id.vars = c("sample", "t")
  )
  return(
    ggplot(long.dt) +
    aes(t, value, color = variable, group = interaction(variable, sample)) +
    lapply(compartment.order, function(ly) geom_line(data = function(dt) dt[variable == ly], alpha = alph)) +
    scale_x_simtime() +
    scale_y_continuous(name = NULL) +
    scale_color_compartment() +
    theme_minimal() +
    theme(legend.position = c(1, 0.5), legend.justification = c(1, 0.5))
  )
}

# TODO support a `...`s version?

#' @title Patchwork Grid
#'
#' @description Provides a quick way to convert a named list structure
#' (possibly nested) of plots into a tagged grid arrangement of those plots.
#'
#' @param plist a list of named lists of [ggplot2] objects, representing a set
#' of plots to layout as a grid.
#'
#' @export
patchwork_grid <- function(plist) {
  # TODO more checks
  stopifnot(
    "`plist` not a list." = is.list(plist),
    "Empty `plist`." = length(plist) != 0
  )

  return(plist |> lapply(\(rw) {
    mapply(\(p, nm) {
      wrap_elements(p + plot_annotation(title = nm))
    }, rw, names(rw), SIMPLIFY = FALSE) |> Reduce(`|`, x = _)
  }) |> Reduce(`/`, x = _))

}
