
require(MTM)

#' REMINDER: Your TODOs are marked with Q: ...
#' There are some items marked with SIDEQ: ...
#' Do NOT consider those until you've gotten to the end of the practical
#' and A'd all the Q's.

#' in the MTM package, we provide several functions to support this exercise
#' you can see their contents by supplying the function name at the console
#' prompt, without the parentheses. e.g.:

network_build

#' Q: Recalling the definitions from the introductory and Networks MTM sessions,
#' what Reed Frost model *variables* & *parameters* appear in `network_build`?
#' Which aspects of the Reed-Frost model are represented here?
#'
#' A:
#' variables: S & I (no R yet)
#' parameters: N (no p)

#' Q: Now consider: what Reed Frost variables & parameters are needed for state update?
#' You can check your answer by considering `network_next_state`
#' A: Now need all the variables (S, I, & R), as well as p from parameters.
#' Unlike some versions of SIR, we do NOT consider the population size N,
#' during the update, but it is implicitly present, via what links exist

network_next_state

#' Q: Think back to the idea of a "model iteration loop" concept motivating
#' earlier sessions, how does Reed-Frost work? Which of the loop constructs,
#' `for` vs `while` is appropriate? Why?
#'
#' Check your intuition against `network_run_reed_frost` and the function used
#' in it's loop condition.
#'
#' A: Use a `while` loop, and compute as long as there are any infectious individuals

#' Now let's consider an example run of the Reed-Frost model implemented on a
#' network:
sim_example <- network_run_reed_frost(
  # setup a population
  initial_network = network_build(N = 30, p = 0.05),
)

#' we can look at the resulting epidemic in summary
#' :
state_example <- network_flatten_run(sim_example)
state_example

samples.dt <- network_sample_reed_frost(n=1000, N=30, p=0.1)

network_plot_series(samples.dt)

network_plot_histograms(samples.dt)


#' produce animation of network record along side a state record time series

#' first do a single network to get a feel what's conceptual framework

#' step 2, do a bunch of samples, look at duration + final size plot


#' Q: what do you notice about these distributions?
#' want to elicit that there is extinction (close to zero final size lump) + there are outbreaks (bigger, non-zero lump)
#' and those vary in size + relationship to duration of epidemic (generally larger => longer?)

#' ask them to do different things with parameters

#' Q: vary p, while holding N constant - what does that do to distribution?
#' p lower => more in the extinction lump, p higher more in the epidemic lump, and epidemic lump pushed higher (though limited by N)
#' TBD re time

...repeat code from earlier question, but indicate they should change things and run it multiple times to look at pictures

#' Q: vary N, holding p constant - what does that do to distribution?
#' Obviously, larger N => larger final sizes. Less obviously: more epidemics.
#' To do with holding *individual* probability constant & increasing N => increasing probability of tranmission
#' since everyone is connected

...repeat code from earlier question, but indicate they should change things and run it multiple times to look at pictures

#' bonus-y question:
#' Q: what constraint on N and p would you have to impose to retain the same shape of the final size
#' distribution with varying N? Hint: how might you have an R0-like concept in this model?

...provide skeleton not code here

