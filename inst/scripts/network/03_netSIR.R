
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

# Let's start with how these networks are the *same*: each pair has the same
# population, and the same number of edges:

network_unstructured_set
network_structured_set

# ... but compare any particular pair; what do you see?
pick <- 42
list(list(
  "Unstructured" = network_unstructured_set[[pick]] |> network_quickplot(simple = TRUE),
  "Structured" = network_structured_set[[pick]] |> network_quickplot(simple = TRUE)
)) |> patchwork_grid()

# That picture might be a bit unclear; if we relax the network layout, the
# difference should be clearer:
list(list(
  "Unstructured" = network_unstructured_set[[pick]] |> add_layout_(with_fr()) |>
    network_quickplot(simple = TRUE),
  "Structured" = network_structured_set[[pick]] |> add_layout_(with_fr()) |>
    network_quickplot(simple = TRUE)
)) |> patchwork_grid()

#' @question How would you describe the difference between these networks?
#'
#' @answer For any given pair ...
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

#' @question Comparing the two distinct network classes, what do you notice?
#'
#' @answer
#'
#' @question How do you imagine these networks were constructed, in terms of
#' the basic [igraph] primitives we reviewed in the warmup?
#'
#' @answer
