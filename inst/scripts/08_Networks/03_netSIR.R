
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

# However, if you compare any particular pair, you might notice differences:
pick <- 42
list(list(
  "Unstructured" = network_unstructured_set[[pick]] |>
    network_quickplot(simple = TRUE),
  "Structured" = network_structured_set[[pick]] |>
    network_quickplot(simple = TRUE)
)) |> patchwork_grid()

# There are some different edges, but how different? Now we'll use one of the
# super-powers of [igraph] (and other network libraries): their layout engine.
#
# If we remove the default layout we enforced, the difference becomes clearer:
list(list(
  "Unstructured" = network_unstructured_set[[pick]] |> add_layout_(with_fr()) |>
    network_quickplot(simple = TRUE),
  "Structured" = network_structured_set[[pick]] |> add_layout_(with_fr()) |>
    network_quickplot(simple = TRUE)
)) |> patchwork_grid()

#' @question Repeating the above for a few different `pick`s, describe the
#' difference between these networks.
#'
#' @answer
#' 
#' 
#' @question Guess how this distinction will affect the final size and epidemic
#' duration outcomes (be sure to recall the previous distributions).
#'
#' @answer
#' 
#' 
#' 
#' 
#' @section Comparing Outcomes
#'
#' Now let's have a look at the results of solving SIR on these networks ...

unstructured.dt <- network_unstructured_set |> lapply(
  network_solve, parms = list(N = 50, p = 0.05)
) |> lapply(network_flatten) |> rbindlist(idcol = "sample")

structured.dt <- network_structured_set |> lapply(
  network_solve, parms = list(N = 50, p = 0.05)
) |> lapply(network_flatten) |> rbindlist(idcol = "sample")

list(list(
  "Unstructured" = unstructured.dt |> network_plot_histograms(),
  "Structured" = structured.dt |> network_plot_histograms()
)) |> patchwork_grid()

#' @question Comparing the two distinct network classes, what do you notice? Can
#' you explain the difference?
#'
#' @answer
#' 
#' 
#' 
#' 
#' @question How do you imagine these networks were constructed, in terms of
#' the basic [igraph] primitives we reviewed in the warmup?
#'
#' @answer
#' 
#' 
#' 
