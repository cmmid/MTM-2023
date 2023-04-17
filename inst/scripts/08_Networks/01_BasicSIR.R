
require(MTM)
require(igraph)

reminder(
"This material is written using the `igraph` library. There are other libraries
that provide the same basic functionality, but via different approaches,
e.g. `networkx`."
)

#' @section The Reed Frost Model on a Network
#'
#' We have used the tools from the Warmup to implement the Reed Frost SIR model
#' (as covered in the discussion session).  In this practical, we'll apply that
#' implementation and and examine some results.
#'
#' We start with the following function that collects several
#' [igraph] steps to initialize the kind of networks we'll
#' use for simulation:

network_build

#' @question What kind of network are we making - deterministic or stochastic?
#' How can you tell? How does the choice relate to the Reed Frost model?
#'
#' @answer
#'
#'
#'
#'
#' @question Recalling the definitions from the Introductory and
#' Networks MTM sessions, what Reed Frost model *variables* &
#' *parameters* appear in `network_build`? Which aspects of the
#' Reed-Frost model are represented here?
#'
#' @answer
#'
#'
#'
#'
#'
#'
#' @question Given [network_build()] and how we've specified the Reed
#' Frost model, how might the variables & parameters be used for
#' for the "delta" meta-modelling step (i.e., calculating system
#' changes)? What functions and operators from `igraph` do you
#' expect to use?
#'
#' @answer
#'
#'
#'
#'
#' @hint compare your thinking to the implementation in `network_dReedFrost`:

network_dReedFrost

#' @question Recall the "model iteration" concept discussed in
#' earlier sessions. Which of the loop constructs `for` vs `while`
#' is appropriate for Reed Frost? Why?
#'
#' @answer
#'
#'
#'
#' @hint Examine `network_solve` to see what's used here.

network_solve

set.seed(13)

sim_example <- network_solve(parms = list(N = 30, p = 0.05))

# we can look at the resulting epidemic in summary
network_plot_series(network_flatten(sim_example))
# or watch its evolution on the network:
# n.b. the rendering here may take a moment
network_animate(sim_example)

# our Reed-Frost model is stochastic, so we can get very different
# results, e.g.:

set.seed(42)

network_plot_series(network_flatten(network_solve(parms = list(N = 30, p = 0.05))))

# ...which means we need to think about typical behavior
# across many realizations of the simulation
# n.b.: this may take a minute
samples.dt <- network_sample_ReedFrost(n = 300, parms = list(N = 30, p = 0.1))

# we can get a holistic sense of the trends in these realizations
# by overlaying the time series
network_plot_series(samples.dt)

# but we generally have some particular features in mind when
# doing this kind of modelling work, e.g. final size or epidemic
# duration
network_plot_histograms(samples.dt)

#' @question What do you notice about these distributions?
#'
#' @answer
#'
#'
#'
#'
#' @section Comparison to Non-network Reed-Frost:
#'
#' We have deliberately constructed this initial application of the network
#' tools to reproduce a model we could implement using more direct base `R`
#' functions. Let's have a look at the step function for the "chain binomial"
#' implementation of Reed-Frost:

# note: this is named with another session, because it is a stochastic, discrete
# model!
stochdisc_dReedFrost

#' ... and if we apply use that step function repeatedly, for many samples, we
#' can build up a distribution of outcomes like we did for the network version,
#' and then compare those results:

cb_samples.dt <- stochdisc_sample(n = 300, parms = list(N = 30, p = 0.1))

patchwork_grid(list(list(
  "Network Histograms" = network_plot_histograms(samples.dt),
  "Chain-Binomial Histograms" = network_plot_histograms(cb_samples.dt)
)))

#' @question How do these outcomes compare?
#'
#' @answer
#'
#' @section Closing Considerations
#'
#' Time permitting, let's think a little bit about this model, ignoring for a
#' moment it's network-ness.
#'
#' @question Using the code from earlier, vary p, while holding N constant, and
#' examine the results with the histogram plot. What happens with the
#' distribution? Why?
#'
#' @hint You may want to use the chain binomial version, since it runs faster
#' and produces the same results.
#'
#' @answer
#'
#'
#'
#'
#'
#'
#'
#' @question Again using code from earlier, now vary $N$, holding $p$ constant:
#' What does that do to distribution? Why?
#'
#' @answer
#'
#'
#'
#'
#'
#'
#' @aside
#'
#' @question what constraint on $N$ and $p$ could impose to get some kind of
#' "consistent" features while varying $N$ or $p$? What kinds of "consistent"
#' can be achieved?
#'
#' @hint how might you have an R0-like concept in this model?
#'
#' @answer
#'
#'
#'
#'
#'
#'
#' @examples
#' samples60.dt <- network_sample_ReedFrost(n=100, list(N=60, p=30/60*0.1))
#' samples120.dt <- network_sample_ReedFrost(n=100, list(N=120, p=30/120*0.1))
#'
#' network_plot_histograms(samples60.dt)
#' network_plot_histograms(samples120.dt)
