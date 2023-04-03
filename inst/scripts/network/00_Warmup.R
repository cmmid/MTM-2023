
require(MTM)        # for overall course functions
require(igraph)     # for the network capabilities
require(ggplot2)    # for plotting

# you may not even need these, but they could be
# useful if you want to do some ad hoc plotting
require(patchwork)  # for combining plots

reminder(
"This material is written using the [igraph] library. There are other libraries
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
#' people on a contact grid. In one, people are randomly vaccinated; the other,
#' in a systematic way.

# set some vertex and edge colors
demo.colors <- c(
  vaccinated = "blue", unvaccinated = "yellow",
  transmissible = "grey", blocked = "transparent"
)

# plot the two networks side-by-side (`network_warmup_vaccine...` - data
# included in `MTM`, as are the `network_quickplot` and `patchwork_grid`
# functions)
list(list(
  # random vaccination in a small, orderly population
  "Random\nVaccination" = network_quickplot(
    network_warmup_vaccine_random, values = demo.colors
  ),
  # targetted vaccination in same
  "Targetted\nVaccination" = network_quickplot(
    network_warmup_vaccine_ordered, values = demo.colors
  )
)) |> patchwork_grid()

#' These plots highlight an important insight from the discussion portion of the
#' session: that relationships can matter. Now let's work through the functions
#' of the [igraph] library to build-up networks like these as well as those we
#' will use in the later exercises.
#'
#' @aside The [network_quickplot()] function is how you will visualize networks
#' throughout these practicals. This function streamlines supplying arguments
#' to a several [ggplot2] functions and returns the resulting plot object. The
#' function [patchwork_grid()] lays out multiple plots for comparison. You can
#' see the implementation of these and other functions by entering them at the
#' `R` prompt without parentheses, or examine their documentation with `?` or
#' the "Help" tab in Rstudio.
#'
#' @section Basic Structures
#'
#' the [igraph] library has several functions to create common network
#' structures, including both deterministic ones (e.g. connected graphs and
#' lattices) and probabilistic generators (e.g. Erdos-Renyi random graph).
#'
#' In [igraph], the general convention is that the deterministic functions start
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
#' @answer
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' @question Of the three generators, `make_full_graph()`, `make_lattice()`, and
#' `sample_gnp()`, which do you think is behind the plots we first looked at?
#'
#' @answer
#' 
#' 
#' 
#' @hint look at `?make_full_graph`, `?make_lattice`, and `?sample_gnp`.
#'
#' @aside Note that we are adding layouts to the plots above. We do that to
#' ensure getting particular plots; without them, each plot of an igraph object
#' gives slightly different arrangements of vertices and edges. For example:

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
)) |> patchwork_grid() & geom_text( # add some labels for convenience
  aes(
    x=(x1+x2)/2, y=(y1+y2)/2, label = eid
  ), data = network_edge_data(igBl)
)

#' @question This is starting to look like the initial networks we plotted.
#' What collection of edges could we delete to isolate the diagonal set of
#' vertices?
#'
#' @answer
#' 
#' 
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
  aes(x=x + .1, y=y, label = vid),
  data = network_vertex_data(iglstar)
)

#' @question What are some other approaches to add edges in [igraph]?
#'
#' @answer
#' 
#' 
#' 
#' @hint check `?add_edges`
#'
#' Now let's combine some whole graphs. There are two basic ways to do this:
#' using the `+` operator between two graphs and using [igraph::graph.union()]
#'
#' Let's have a look at what those do:

iglstar <- make_star(9, mode = "undirected") |> add_layout_(as_star())
iglring <- make_ring(9, directed = FALSE) |> add_layout_(in_circle())

list(list(
  "Addition" = network_quickplot(iglstar + iglring, simple = TRUE),
  "Union" = network_quickplot(graph.union(iglstar, iglring), simple = TRUE)
)) |> patchwork_grid()

#' @question What is the difference between [igraph::graph.union()] and
#' using the `+` operator?
#'
#' @answer
#' 
#' 
#' @section Modifying Networks, part 2
#'
#' The previous section introduced directly modifying networks: adding and/or
#' deleting edges and vertices. We *could* use this approach to make the
#' plots from the top of this practical.
#'
#' In those networks, we have some _vaccinated_ and _unvaccinated_
#' individuals, and the edges (over which transmission occurs) connecting
#' vaccinated individuals do not appear--but not because they have been deleted!
#' The model here is not that these people no longer contact the unvaccinated
#' population, simply that transmission does not occur along those contacts.
#'
#' We can create plots (and more importantly, simulations) that maintain these
#' interactions while *also* capturing that this transmission route is
#' blocked. To do so, we typically modify the attributes of edges and vertices,
#' often based on the attributes their connected edges and vertices.
#'
#' In [igraph], these attributes can be conveniently queried and modified
#' using the [igraph::V()] and [igraph::E()] functions for accessing the
#' *V*ertices and *E*dges that match the properties in the `V(network)[...]`
#' square braces (much like using [which()] or [subset()] from base R).
#'
#' In this section, we illustrate some of the functionality of [igraph::V()] and
#' [igraph::E()] to recreate those two initial graphs

# first, we make two initially identical populations - 100 individuals, in
# 10x10 lattices, everyone initially unvaccinated:
ordered.pop <- make_lattice(length=10, dim=2) |> add_layout_(on_grid())
V(ordered.pop)$state <- "unvaccinated"
# makes a copy with identical attributes:
random.pop <- ordered.pop

# then select the vaccinees: first, those individuals on the diagonal of
# the `ordered` lattice ...
orderedvaccinees <- seq(1, 100, by = 11)
# and the same number of random individuals. note that we set the seed - this
# is the one we used to generate the network provided with the MTM package
set.seed(8675309)
randomvaccinees <- sample(100, length(orderedvaccinees), replace = FALSE)

# then, the apply these selections:
V(ordered.pop)$state[orderedvaccinees] <- "vaccinated"
V(random.pop)$state[randomvaccinees]   <- "vaccinated"

# Now we can use this state to get subsets of the networks, e.g.:
V(ordered.pop)[state == "vaccinated"]
V(ordered.pop)[state != "vaccinated"]

# We can similarly set attributes for edges, and now we'll use our vertex states
# to identify which edges to change:
E(ordered.pop)$state <- "transmissible"
E(ordered.pop)[
  .inc(V(ordered.pop)[state == "vaccinated"])
]$state <- "blocked"
E(random.pop)$state <- "transmissible"
E(random.pop)[
  .inc(V(random.pop)[state == "vaccinated"])
]$state <- "blocked"
# `.inc` here is a special function in `igraph` - it means "Edges *inc*ident on
# this set of vertices." Look at `?igraph-es-indexing` to see others.

# let's see how these all compare:
demo.colors <- c(
  vaccinated = "blue", unvaccinated = "yellow",
  transmissible = "grey", blocked = "transparent"
)
# recreate this just in case it got lost

# now plot your two networks alongside the MTM ones ...
list(list(
  "MTM Random" = network_quickplot(network_warmup_vaccine_random, values = demo.colors),
  "MTM Targetted" = network_quickplot(network_warmup_vaccine_ordered, values = demo.colors)
), list(
  "My Random" = network_quickplot(random.pop, values = demo.colors),
  "My Targetted" = network_quickplot(ordered.pop, values = demo.colors)
)) |> patchwork_grid()
# they should be identical. If you were to recreate the `random.pop`, however:

randomvaccinees <- sample(100, length(orderedvaccinees), replace = FALSE)
V(random.pop)$state <- "unvaccinated"
V(random.pop)$state[randomvaccinees] <- "vaccinated"
E(random.pop)$state <- "transmissible"
E(random.pop)[
  .inc(V(random.pop)[state == "vaccinated"])
]$state <- "blocked"

# ... you'll find a different distribution, because the random seed differs
list(list(
  "MTM Random" = network_quickplot(network_warmup_vaccine_random, values = demo.colors),
  "MTM Targetted" = network_quickplot(network_warmup_vaccine_ordered, values = demo.colors)
), list(
  "My Random" = network_quickplot(random.pop, values = demo.colors),
  "My Targetted" = network_quickplot(ordered.pop, values = demo.colors)
)) |> patchwork_grid()

#' @question In this section we introduced [igraph::E()] and [igraph::V()] for
#' "indexing" edge and vertex sets. What is the special function we introduced?
#' What are some other special functions for indexing operations?
#'
#' @answer
#' 
#' 
#' @aside Note the code repetition here; we used functions instead when building
#' the objects for the package. Can you write functions that eliminate the
#' repetition? Check your work against the MTM package source on github!
#'
#' @section Modifying Networks, part 3
#'
#' Now that we have covered some [igraph] basics, particularly using network
#' attributes, we'll think about how we might use these to represent a
#' *S*usceptible-*I*nfectious-*R*ecovered epidemic on a network.
#'
#' Recall the *SIR* model world from earlier sessions: essentially, when *S*s
#' and *I*s interact, the *S* individuals may change state to *I*. After some
#' time, *I* individuals become *R*, both no longer transmitting and immune to
#' future infection. The dynamics are characterized by $R_0$: the typical number
#' of new *I*s a single *I* produces, when introduced to an otherwise completely
#' *S* population.
#'
#' Let's use our vaccine lattice to briefly consider a way to represent the
#' *SIR* model world.

# first, let's make a function to translate our vaccinated & unvaccinated
# networks
popsetup <- function(ig) {
  V(ig)[state != "vaccinated"]$state <- "S"
  infectee <- sample(V(ig)[state == "S"], 1)
  V(ig)[infectee]$state <- "I"
  ig
}

# now let's make a function to:
#  - check possible transmission routes
#  - transmit randomly along some of them
#  - and then cause infections to recover
poptransmit <- function(ig, prob = 0.5) {
  possibleroutes <- E(ig)[V(ig)[state == "S"] %--% V(ig)[state == "I"]]
  actualroutes <- possibleroutes[runif(length(possibleroutes)) < prob]
  V(ig)[state == "I"]$state <- "R"
  V(ig)[(state == "S") & .inc(actualroutes)]$state <- "I"
  E(ig)[.inc(V(ig)[state == "R"])]$state <- "blocked"
  E(ig)[actualroutes]$state <- "active"
  ig
}

#' @question In those two functions, we've introduced a few new capabilities.
#' What did you notice?
#'
#' @answer
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' @section Modifying Networks, part 3, cont:

# now setup the populations, run a few steps, and plot
ordered.pop <- popsetup(network_warmup_vaccine_ordered)
ordered.pop1 <- poptransmit(ordered.pop)
ordered.pop2 <- poptransmit(ordered.pop1)
ordered.pop3 <- poptransmit(ordered.pop2)
ordered.pop4 <- poptransmit(ordered.pop3)

random.pop <- popsetup(network_warmup_vaccine_random)
random.pop1 <- poptransmit(random.pop)
random.pop2 <- poptransmit(random.pop1)
random.pop3 <- poptransmit(random.pop2)
random.pop4 <- poptransmit(random.pop3)

sir.cols <- c(
  vaccinated = "blue", S = "yellow", I = "firebrick", R = "darkgreen",
  transmissible = "grey", blocked = "transparent", active = "red"
)

list(list(
  "t=0" = network_quickplot(ordered.pop, values = sir.cols),
  "t=1" = network_quickplot(ordered.pop1, values = sir.cols),
  "t=2" = network_quickplot(ordered.pop2, values = sir.cols),
  "t=3" = network_quickplot(ordered.pop3, values = sir.cols),
  "t=4" = network_quickplot(ordered.pop4, values = sir.cols)
), list(
  "t=0" = network_quickplot(random.pop, values = sir.cols),
  "t=1" = network_quickplot(random.pop1, values = sir.cols),
  "t=2" = network_quickplot(random.pop2, values = sir.cols),
  "t=3" = network_quickplot(random.pop3, values = sir.cols),
  "t=4" = network_quickplot(random.pop4, values = sir.cols)
)) |> patchwork_grid()

#' @question Try running those previous simulate + plot lines a few times.
#' What kind of patterns do you see emerging?
#'
#' @answer
#' 
#' 
#' 
#' 
#' @section Follow Up Work
#'
#' We only touched the surface of the capabilities of the [igraph] library.
#' There are many other network generation functions (try `igraph::make_` or
#' `igraph::sample_` and then TAB to see suggestions for others), as well as
#' functions to save and load networks in various formats.
#'
#' [igraph] also has a wealth of analytical tools for computing various metrics.
#' We do not use those in this session, but they are often part of evaluations
#' to try to understand why simulations on different networks have different
#' features, or to predict what kind of epidemic outcomes will result from
#' different network properties.
