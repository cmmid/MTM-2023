
require(MTM)
require(igraph)

reminder(
"This material is written using the `igraph` library. There are other libraries
that provide the same basic functionality, but via different approaches,
e.g. `networkx`."
)

#' @section Reed Frost Model on a (different) Network
#'
#' In the previous practical, we used networks, but there wasn't
#' actually much structure - each individual in the population was
#' connected to everyone else. That effectively results in the same
#' outcome as the Reed-Frost model *without* any network structure.
#'
#' In this exercise, we will consider randomly percolated networks:

# setting the seed so that our previous network is identical the first one used
# in the previous practical
set.seed(13)

pars <- list(N = 30, p = 0.05)
previous_network <- pars |> network_build()
new_network <- network_percolate(pars, previous_network)

list(list(
  "Reference Reed-Frost\nNetwork" = network_quickplot(previous_network),
  "Percolated\nNetwork" = network_quickplot(new_network)
)) |> patchwork_grid()

#' @question What differs between [network_build()] and [network_percolate()]?
#'
#' @answer
#'
#'
#'
#' @question The percolated network is much less dense, but now transmission is
#' guaranteed on the remaining edges. How do you think this will effect the
#' dynamics? What are some features we might check to see those differences?
#'
#' @answer
#'
#'
#'
#'

#' @section Comparing the Reference and Percolated Networks
#'
#' Using the exact same underlying modelling functions for transmission, we're
#' going to run both the reference and percolated networks, and compare the
#' outcomes.
#'
#' Make a guess as to what will happen (different, basically the same, ...?) and
#' then try it out.

previous_example <- network_solve(y = previous_network, parms = pars)
new_example <- network_solve(y = new_network, parms = pars)

previous_plot <- previous_example |> network_flatten() |> network_plot_series()
new_plot <- new_example |> network_flatten() |> network_plot_series()

list(list(
  "Reference Results" = previous_plot,
  "Percolated Results" = new_plot
)) |> patchwork_grid()

#' @question The networks are very different; why are those plots identical?
#'
#' @answer
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @section Reed Frost Model on a (different) Network, part II
#'
#' Before we jump to the final exercise, let's briefly investigate the
#' comparison between networks that share the same parameters, but haven't been
#' made to precisely match.

prev_samples.dt <- network_sample_ReedFrost(n = 100, parms = pars, ref_seed = 5)
new_samples.dt <- network_sample_ReedFrost(
  n = 100, parms = pars, setup_fun = network_percolate
)

list(list(
  "Reference Histograms" = prev_samples.dt |> network_plot_histograms(),
  "Percolated Histograms" = new_samples.dt |> network_plot_histograms()
)) |> patchwork_grid()

#' @question How do these compare?
#'
#' @answer
#'
#' @aside Feel free to play around with elements like the number of samples,
#' the size of the networks, probability of transmission - what happens to the
#' similarity between these plots with respect to these sort of changes? How
#' does that comport with your observations in the previous section?
