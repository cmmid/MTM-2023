
require(MTM)
require(data.table)

#' @section Reed Frost Model on a (*actually* different) Network
#'
#' In the previous practical, we saw that some kinds of structure don't
#' necessarily make a difference. The randomly connected individuals actually
#' lead to the same epidemic characteristics - even the precise same epidemics!
#'
#' So why bother with network models? They are mainly useful when there is some
#' *discernable* structure.
#'
#' For this final exercise, we have pre-made some networks with structure, for
#' comparison with the "structured" networks in the previous exercise.
#'
#' Let's start with how these networks are the *same*. Each pair has the same
#' population, and the same number of edges. These are all size 50 networks,
#' though they have varying edges counts because they were constructed randomly.
#' In this case, they all have p = typical edges / all possible edges = 0.04.

all(
  sapply(network_unstructured_set, vcount) ==
  sapply(network_structured_set, vcount)
)

all(
  sapply(network_unstructured_set, ecount) ==
  sapply(network_structured_set, ecount)
)

# However, if you compare any particular pair, you might notice differences:
pick <- 42
patchwork_grid(list(list(
  "Unstructured" = network_quickplot(
    network_unstructured_set[[pick]], simple = TRUE
  ),
  "Structured" = network_quickplot(
    network_structured_set[[pick]], simple = TRUE
  )
)))

# There are some different edges, but how different? Now we'll use one of the
# super-powers of [igraph] (and other network libraries): their layout engine.
#
# If we remove the default layout we enforced, the difference becomes clearer:
patchwork_grid(list(list(
  "Unstructured" = network_quickplot(
    add_layout_(network_unstructured_set[[pick]], with_fr()), simple = TRUE
  ),
  "Structured" = network_quickplot(
    add_layout_(network_structured_set[[pick]], with_fr()), simple = TRUE
  )
)))

#' @question Repeating the above for a few different `pick`s, describe the
#' difference between these networks.
#'
#' @answer For each pair, the structured network tends to have two clumps.
#' Sometimes those are connected, sometimes not.
#'
#' @question Guess how this distinction will affect the final size and epidemic
#' duration outcomes (be sure to recall the previous distributions).
#'
#' @answer For the unstructured networks previously, we tended to get a bimodal
#' distribution of short, small epidemics and longer, most-of-the-network
#' epidemics. For the structured networks, we'll probably see the same, but with
#' an intermediate cases for when the lumps aren't connected.
#'
#' @section Comparing Outcomes
#'
#' Now let's have a look at the results of solving SIR on these networks ...

unstructured.dt <- rbindlist(lapply(lapply(
  network_unstructured_set,
  network_solve, parms = list(N = 50, p = 0.05)
), network_flatten), idcol = "sample")

structured.dt <- rbindlist(lapply(lapply(
  network_structured_set,
  network_solve, parms = list(N = 50, p = 0.05)
), network_flatten), idcol = "sample")

patchwork_grid(list(list(
  "Unstructured" = network_plot_histograms(unstructured.dt),
  "Structured" = network_plot_histograms(structured.dt)
)))

#' @question Comparing the two distinct network classes, what do you notice? Can
#' you explain the difference?
#'
#' @answer As guessed earlier, there is an intermediate case. If we imagine the
#' two lumps as "households", sometimes the infection never takes off, sometimes
#' it takes off in one household, and sometimes its able to jump from one
#' household to the other.
#'
#' @question How do you imagine these networks were constructed, in terms of
#' the basic [igraph] primitives we reviewed in the warmup?
#'
#' @answer (You can look at the package repository, in the `data-raw/networks.R`
#' script for the exact answer). Roughly, build *two* half-the-target-size
#' networks, percolate them, add them together, and then introduce one random
#' edge between the two. This random connection might connect big clumps or not.
