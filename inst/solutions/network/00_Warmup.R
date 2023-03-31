
require(MTM)        # for overall course functions
require(igraph)     # for the network capabilities
require(ggplot2)    # for plotting

# you may not even need these, but they could be
# useful if you want to do some adhoc plotting
require(patchwork)  # for combining plots

reminder(
"This material is written using the `igraph` library. There are other libraries
that provide the same basic functionality, but via different approaches,
e.g. `networkx`."
)

#' @section Motivation
#'
#' Network-based models (aka, graphs) are useful when the system you are trying
#' to capture has lots of relationship structure that needs to be represented
#' to accurately reproduce what is going on.
#'
#' In this warmup, we're going to show you how to build up two networks, both of
#' people on a contact grid. In one, people are vaccinated at random; in the other, in
#' a systematic way.

# set some vertex and edge colors
demo.cols <- c(
  vaccinated = "blue", unvaccinated = "yellow",
  transmissible = "grey", blocked = "transparent"
)

# plot the two networks side-by-side (`network_warmup_vaccine...` - data
# included in `MTM`, as are the `network_quickplot` and `patchwork_grid`
# functions)
list(list(
  # random vaccination in a small, orderly population
  "Random\nVaccination" = network_quickplot(
    network_warmup_vaccine_random, values = demo.cols
  ),
  # targetted vaccination in same
  "Targetted\nVaccination" = network_quickplot(
    network_warmup_vaccine_ordered, values = demo.cols
  )
)) |> patchwork_grid()

#' These plots highlight an important insight from the discussion portion of the
#' session: that relationships can matter. Now let's work through the functions
#' of the `igraph` library to build-up networks like these as well as those we
#' will use in the later exercises.
#'
#' @aside The [network_quickplot()] function is what you will use throughout these
#' practicals to visualize networks. This function streamlines supplying arguments
#' to a several [ggplot2] functions and returns the resulting [ggplot2::ggplot]
#' object. You may recall [patchwork_grid()] from previous sessions where you
#' were laying out multiple plots for comparison; we use it here in the same way.

#' @section Basic Structures
#'
#' the `igraph` library has several functions to create common network
#' structures, including both deterministic ones (e.g. connected graphs and
#' lattices) and probabilistic generators (e.g. Erdos-Renyi random graph).
#'
#' In `igraph`, the general convention is that the deterministic functions start
#' with `make_...`, and the probabilistic generators start with `sample_...`

# make several different kinds of graphs, at two sizesm L(ittle) = 9, B(ig) = 25
igL <- make_full_graph(n = 9) |> add_layout_(with_graphopt())
igB <- make_full_graph(n = 25) |> add_layout_(with_graphopt())
igLgnp <- sample_gnp(n = 9, p = 0.2) |> set_graph_attr("layout", igL$layout)
igBgnp <- sample_gnp(n = 25, p = 0.2) |> set_graph_attr("layout", igB$layout)
igLl <- make_lattice(length = 3, dim = 2) |> add_layout_(on_grid())
igBl <- make_lattice(length = 5, dim = 2) |> add_layout_(on_grid())
# we use `add_layout()` for visualization purposes - it has no other effects

# feel free to inspect those objects if you like, but you can get the overall
# point by plotting them alongside each other:
list(list(
  "N=9 Clique"  = network_quickplot(igL, simple = TRUE),
  "N=25 Clique" = network_quickplot(igB, simple = TRUE)
), list(
  "N=9 Random"  = network_quickplot(igLgnp, simple = TRUE),
  "N=25 Random" = network_quickplot(igBgnp, simple = TRUE)
), list(
  "N=9 Lattice"  = network_quickplot(igLl, simple = TRUE),
  "N=25 Lattice" = network_quickplot(igBl, simple = TRUE)
)) |> patchwork_grid()

#' @question How would you describe the difference between the graphs created by
#' `make_full_graph()` vs. `sample_gnp()` vs. `make_lattice()`?
#'
#' @answer The `make_...` generators produce graphs with fixed properties; all
#' vertices connected for `make_full_graph()` and vertices connected in a grid
#' for `make_lattice()` (a typical 2D one for the basic arguments, or more
#' complicated grids if you fiddled around with `dim=` arguments).
#' `sample_gnp()` produces a random set of edges amongst the vertices. If you
#' look at another person's plots, they will likely have different connections.
#' (You may also notice slightly different layouts, particularly for the "B"
#' networks.)
#'
#' @question Of the three generators, `make_full_graph()`, `make_lattice()`, and
#' `sample_gnp()`, which do you think is behind the plots we first looked at?
#'
#' @answer The example networks above are made with `make_lattice()`, with some
#' other modifications. In the later exercises, we will use `make_full_graph()`,
#' and then make modifications.
#'
#' @hint look at `?make_full_graph`, `?make_lattice`, and `?sample_gnp`.
#'
#' @aside Note that we are adding layouts to the plots above. We do that to
#' ensure getting particular plots; without them, each plot of an igraph object
#' gives different arrangements of vertices and edges. For example:

list(list(
  "Original" = network_quickplot(igL, simple = TRUE),
  "...Another" = network_quickplot(make_full_graph(n = 9), simple = TRUE),
  "& Another" = network_quickplot(make_full_graph(n = 9), simple = TRUE)
)) |> patchwork_grid()

#' @section Modifying Networks, part 1
#'
#' In the previous section, we built some foundational networks using the tools
#' in [igraph]. But those had none of the attributes that we discussed as part
#' of the module or that can be seen as part of the initial networks we looked
#' at in this practical.
#'
#' To make those networks, we need to use some of the other [igraph] tools for
#' adding and deleting vertices and edges, and for assigning properties to edges
#' and vertices.

# delete every other edge on the lattice
igBl <- make_lattice(length = 5, dim = 2) |> add_layout_(on_grid())
igBlmod <- igBl |> delete_edges(1:(ecount(igBl)/2)*2)
# repeated definition of igBl, in case it got deleted/changed/etc

list(list(
  "N=25 Lattice" = network_quickplot(igBl, simple = TRUE),
  "N=25 Lattice--" = network_quickplot(igBlmod, simple = TRUE)
)) |> patchwork_grid() & geom_text(
  aes(
    x=(x1+x2)/2, y=(y1+y2)/2, label = eid
  ), data = network_edge_data(igBl)
)

#' @question This is starting to look like the initial networks we plotted.
#' What collection of edges could we delete to isolate the diagonal set of
#' vertices?
#'
#' @answer There's a clear pattern of adding `11` to `c(1,2,4,10)` until hitting
#' the top row of vertices, so could do roughly
#' `del <- c(1,2,4,10) + 11*rep(0:3, each=4); del[15:16] <- c(36, 40)`
#'
#' @aside If you wanted to make this work for any size lattice, you have to
#' consider how that the addition increment changes & how to deal with the
#' "last row" switch. For example, consider a slightly larger version of the
#' problem:

igBBl <- make_lattice(length = 7, dim = 2) |> add_layout_(on_grid())
igBBlmod <- igBBl |> delete_edges(1:(ecount(igBBl)/2)*2)
list(list(
  "N=49 Lattice" = network_quickplot(igBBl, simple = TRUE),
  "N=49 Lattice--" = network_quickplot(igBBlmod, simple = TRUE)
)) |> patchwork_grid() & geom_text(
  aes(x=(x1+x2)/2, y=(y1+y2)/2, label = eid), data = network_edge_data(igBBl)
)

#' In a later section, we'll use some of the other [igraph] capabilities to
#' do this more cleverly. For now, let's consider the tools to add vertices
#' and edges to a graph.

# adding edges ...
iglstar <- make_star(9, mode = "undirected") |> add_layout_(as_star())
# can be done by naming specific vertices
iglstarmod <- iglstar |> add_edges(c(c(2,4), c(3,5), c(6,8), c(7,9)))

list(list(
  "N=9 Star" = network_quickplot(iglstar, simple = TRUE),
  "N=9 Star+edges" = network_quickplot(iglstarmod, simple = TRUE)
)) |> patchwork_grid() & geom_text(
  aes(x=x + .1, y=y, label = vid), data = network_vertex_data(iglstar |> as.data.table())
)

#' @question What are some other approaches to add edges in [igraph]?
#'
#' @answer You can use `+` with another [igraph]-like entity, either a whole
#' network or pieces created with [igraph::vertex()], [igraph::vertices()],
#' [igraph::edge()], or [igraph::edges()].
#'
#' @hint check `?add_edges`
