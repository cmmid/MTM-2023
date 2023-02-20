
require(MTM)
require(igraph)

reminder(
"This material is written using the `igraph` library. There are other libraries
that provide the same basic functionality, but via different approaches,
e.g. `networkx`."
)

#' @section The Reed Frost Model on a Network
#'
#' In this practical, we will use the tools from the Warmup to
#' implement the Reed Frost SIR model (as covered in the
#' discussion session), use it, and examine some results.
#'
#' We start with the following function that collects several
#' [igraph] steps to initialize the kind of networks we'll
#' use for simulation:

network_build

#' @question Recalling the definitions from the introductory and
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


#' @question Given [network_build()] and how we've specified the Reed
#' Frost model, how might the variables & parameters be used for
#' for the "delta" meta-modelling step (i.e., calculating system
#' changes)?
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

set.seed(13)

list(N = 30, p = 0.05) |> network_solve(parms = _) -> sim_example

#' we can look at the resulting epidemic in summary
sim_example |> network_flatten() |> network_plot_series()
#' or watch its evolution on the network:
#' n.b. the rendering here may take a moment
sim_example |> network_animate()

#' our Reed-Frost model is stochastic, so we can get very different
#' results, e.g.:

set.seed(42)

list(N = 30, p = 0.05) |> network_solve(parms = _) |>
  network_flatten() |> network_plot_series()

#' ...which means we need to think about typical behavior
#' across many realizations of the simulation
#' n.b.: this may take a minute
samples.dt <- network_sample_ReedFrost(n=300, parms = list(N=30, p=0.1))

#' we can get a holistic sense of the trends in these realizations
#' by overlaying the time series
samples.dt |> network_plot_series()

#' but we generally have some particular features in mind when
#' doing this kind of modelling work, e.g. final size or epidemic
#' duration
samples.dt |> network_plot_histograms()

#' @question What do you notice about these distributions?
#'
#' @answer The results are bi-modal: We see both extinction
#' (close to zero final size lump) and outbreaks (bigger,
#' non-zero lump), which are typically (but not always)
#' attacking almost the entire population.

#' Q: Using the code from earlier, vary p, while holding N constant
#' What does that do to distribution? Why?
#' A: p lower => more in the extinction lump, vs p higher => more in the epidemic lump
#' and epidemic lump pushed higher (though limited by N).

#' Q: Again using code from earlier, now vary N, holding p constant - what does that do to distribution?
#' A: Obviously, larger N => larger final sizes. Less obviously: more epidemics.
#' To do with holding *individual* probability constant & increasing N =>
#' increasing probability of tranmission since everyone is connected
#' Also more *reliable* epidemics as there are more events - think of the binomial distribution
#' more samples => smaller variance

#' ASIDE:
#' Q: what constraint on N and p could impose to get some kind of consistent features while
#' varying N? What kinds of "consistent" can be achieved?
#' Hint: how might you have an R0-like concept in this model?
#' A: If we consider
samples60.dt <- network_sample_reed_frost(n=1000, N=60, p=30/60*0.1)
samples120.dt <- network_sample_reed_frost(n=1000, N=120, p=30/120*0.1)

