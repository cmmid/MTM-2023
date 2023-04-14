
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
#' @answer Deterministic, and can tell from [igraph::make_full_graph()]
#' constructor: deterministic methods start with `make_...`. In the Reed Frost
#' model, "everyone interacts" - same as everyone being connected. But: though
#' the *network* is deterministic, the simulation can still be *stochastic*.
#'
#' @question Recalling the definitions from the Introductory and
#' Networks MTM sessions, what Reed Frost model *variables* &
#' *parameters* appear in `network_build`? Which aspects of the
#' Reed-Frost model are represented here?
#'
#' @answer
#'  - variables: S & I (no R yet really, though it is listed in states)
#'  - parameters: N (N for network size; p isn't used yet)
#'  Which states are present (S, I, R) and infectious (I), as is the
#'  fact that edges may be tested for transmission (draw) and either
#'  lead to transmission or not (active vs inactive)
#'
#' @question Given [network_build()] and how we've specified the Reed
#' Frost model, how might the variables & parameters be used for
#' for the "delta" meta-modelling step (i.e., calculating system
#' changes)? What functions and operators from `igraph` do you
#' expect to use?
#'
#' @answer We'll check for any edges between *S* and *I* individuals.
#' For any we find, we'll test them by comparing `draw` and `parms$p`
#' to see if transmission occurs, which will turn connected *S*s into
#' *I*s. We'll also turn current *I*s into *R*s.
#'
#' @hint compare your thinking to the implementation in `network_dReedFrost`:

network_dReedFrost

#' @question Recall the "model iteration" concept discussed in
#' earlier sessions. Which of the loop constructs `for` vs `while`
#' is appropriate for Reed Frost? Why?
#'
#' @answer Use a `while` loop, computing as long as there are
#' any infectious individuals. The Reed Frost model is a finite,
#' stochastic model, so it has a defined stopping condition.
#'
#' @hint Examine `network_solve` to see what's used here.

network_solve

set.seed(13)

list(N = 30, p = 0.05) |> network_solve(parms = _) -> sim_example

# we can look at the resulting epidemic in summary
sim_example |> network_flatten() |> network_plot_series()
# or watch its evolution on the network:
# n.b. the rendering here may take a moment
sim_example |> network_animate()

# our Reed-Frost model is stochastic, so we can get very different
# results, e.g.:

set.seed(42)

list(N = 30, p = 0.05) |> network_solve(parms = _) |>
  network_flatten() |> network_plot_series()

# ...which means we need to think about typical behavior
# across many realizations of the simulation
# n.b.: this may take a minute
samples.dt <- network_sample_ReedFrost(n=300, parms = list(N=30, p=0.1))

# we can get a holistic sense of the trends in these realizations
# by overlaying the time series
samples.dt |> network_plot_series()

# but we generally have some particular features in mind when
# doing this kind of modelling work, e.g. final size or epidemic
# duration
samples.dt |> network_plot_histograms()

#' @question What do you notice about these distributions?
#'
#' @answer The results are bi-modal: We see both extinction
#' (close to zero final size lump) and outbreaks (bigger,
#' non-zero lump), which are typically (but not always)
#' attacking almost the entire population.
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

cb_samples.dt <- stochdisc_sample(n=300, parms = list(N=30, p=0.1))

list(list(
  "Network Histograms" = samples.dt |> network_plot_histograms(),
  "Chain-Binomial Histograms" = cb_samples.dt |> network_plot_histograms()
)) |> patchwork_grid()

#' @question How do these outcomes compare?
#'
#' @answer They are virtually the same, as planned!
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
#' @answer Generally, $p$ lower => more in the extinction lump, vs $p$ higher =>
#' more in the epidemic lump and epidemic lump pushed higher and earlier,
#' limited by N. With increasing probability of transmission, random extinction
#' before taking off is less likely (shrinking the lump near 0). The durations
#' are generally shorter with increasing $p$ because each generation is
#' typically larger, so the peak (and subsequent decline) can be hit in fewer
#' generations.
#'
#' @question Again using code from earlier, now vary $N$, holding $p$ constant:
#' What does that do to distribution? Why?
#'
#' @answer Larger $N$ allows for larger final sizes. But it also leads to more
#' epidemics (i.e. smaller lump near 0).
#' Holding *individual* transmission probability constant & increasing $N$ =>
#' increasing probability of *some* transmission since everyone is connected.
#' Also more *reliable* epidemics as there are more events: for  binomial
#' distribution more samples => smaller variance.
#'
#' @aside
#'
#' @question what constraint on $N$ and $p$ could impose to get some kind of
#' "consistent" features while varying $N$ or $p$? What kinds of "consistent"
#' can be achieved?
#'
#' @hint how might you have an R0-like concept in this model?
#'
#' @answer Consider how similar the following are in *relative* scale; the only
#' obvious difference is a shift in time - but that's associated with needing
#' more generations to hit the peak. We could express time in a relative scale
#' that would also eliminate this quantitative distinction. The real qualitative
#' distinction between these is the noise around outcomes - smaller populations
#' give more random results.
#'
#' @examples
#' samples60.dt <- network_sample_ReedFrost(n=100, list(N=60, p=30/60*0.1))
#' samples120.dt <- network_sample_ReedFrost(n=100, list(N=120, p=30/120*0.1))
#'
#' samples60.dt |> network_plot_histograms()
#' samples120.dt |> network_plot_histograms()
