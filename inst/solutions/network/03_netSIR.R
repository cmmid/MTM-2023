
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
  network_unstructured_set |> sapply(vcount) ==
    network_structured_set |> sapply(vcount))

all(
  network_unstructured_set |> sapply(ecount) ==
    network_structured_set |> sapply(ecount))

# However, if you compare any particular pair, you might notice some differences:
pick <- 42
list(list(
  "Unstructured" = network_unstructured_set[[pick]] |> network_quickplot(simple = TRUE),
  "Structured" = network_structured_set[[pick]] |> network_quickplot(simple = TRUE)
)) |> patchwork_grid()

# There are some different edges, but how different? Now we'll use one of the
# super-powers of [igraph] (and other network libraries): their layout engine.
#
# If we remove the default network layout we enforced, the difference becomes clearer:
list(list(
  "Unstructured" = network_unstructured_set[[pick]] |> add_layout_(with_fr()) |>
    network_quickplot(simple = TRUE),
  "Structured" = network_structured_set[[pick]] |> add_layout_(with_fr()) |>
    network_quickplot(simple = TRUE)
)) |> patchwork_grid()

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

network_unstructured_set |> lapply(network_solve, parms = list(N = 50, p = 0.05)) |>
  lapply(network_flatten) |> rbindlist(idcol = "sample") -> unstructured.dt

network_structured_set |> lapply(network_solve, parms = list(N = 50, p = 0.05)) |>
  lapply(network_flatten) |> rbindlist(idcol = "sample") -> structured.dt

list(list(
  "Unstructured" = unstructured.dt |> network_plot_histograms(),
  "Structured" = structured.dt |> network_plot_histograms()
)) |> patchwork_grid()

#' @question Comparing the two distinct network classes, what do you notice? Can
#' you explain the difference?
#'
#' @answer As guessed earlier, there is an intermediate case. If we imagine the
#' two lumps as "households", it's as if sometimes the infection never takes off,
#' sometimes it takes off in one household, and sometimes its able to jump from
#' one household to the other.
#'
#' @question How do you imagine these networks were constructed, in terms of
#' the basic [igraph] primitives we reviewed in the warmup?
#'
#' @answer (You can look at the package repository, in the `data-raw/networks.R`
#' script for the exact answer). Roughly, build *two* half-the-target-size
#' networks, percolate them, add them together, and then introduce one random
#' edge between the two. This random connection might connect big clumps or not.
