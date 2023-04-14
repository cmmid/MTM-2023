
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
 name = NULL, values = SIR_colors,
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

#' @title Cartesian Coordinates, 0 to Max
#'
#' @inheritParams ggplot2::coord_cartesian
#' @inheritDotParams ggplot2::coord_cartesian
#'
#' @param xmax alternate parameterization for xlim
#'
#' @param ymax alternate parameterization for ylim
#'
#' @export
coord_max <- function(
  xmax = NA, ymax = NA,
  xlim = c(0, xmax), ylim = c(0, ymax),
  expand = FALSE, ...
) {
  coord_cartesian(xlim = xlim, ylim = ylim, expand = expand, ...)
}

#' @title defines reference colors used in plots throughout MTM course
#'
#' @export
SIR_colors <- c(S = "dodgerblue", I = "firebrick", R = "forestgreen")

#' @title Generic Compartment Plot
#'
#' @description Provides a consistent plot of compartment model results across
#' all sessions.
#'
#' @param dt a [data.table::data.table()] with column `t` (for simulation time),
#' optionally a `sample` column (for stochastic replicates), and overall in wide
#' format (each other column referring to compartment) or long format (having
#' `variable` and `value` columns, with `variable` indicating compartment and
#' `value` the value of that compartment at that time and optionally sample)
#'
#' @param compartment_order a character vector, indicating the layer order for
#' the compartments. N.B. the "first" layer is the first layer drawn (i.e. the
#' bottom-most) and "last" is the last drawn (i.e. the top-most, typically most
#' visible).
#'
#' @details This function is a convenient wrapper to create a
#' [ggplot2::ggplot()] object from typical series data output from example
#' simulations.
#'
#' @export
plot_series <- function(
  dt,
  compartment_order = c("S", "R", "I")
) {
  if (!"sample" %in% colnames(dt)) {
    dt <- as.data.table(dt)[, sample := 1]
  }
  alph <- dt[, 1 / sqrt(length(unique(sample)))]
  long_dt <- melt(
    dt, id.vars = c("sample", "t")
  )
  return(
    ggplot(long_dt) +
    aes(t, value, color = variable, group = interaction(variable, sample)) +
    lapply(compartment_order, function(ly) geom_line(
      data = function(dt) dt[variable == ly], alpha = alph)
    ) +
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
