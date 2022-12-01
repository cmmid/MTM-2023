
require(MTM)        # for overall course functions
require(igraph)     # for the network capabilities
# you may not even need these, but they could be
# useful if you want to do some adhoc plotting
require(ggplot2)    # for plotting
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
#' In this warmup, we're going to show you how to build up these two networks:

# set some vertex and edge colors
demo.cols <- c(
  vaccinated = "blue", unvaccinated = "yellow",
  transmissible = "grey", blocked = "transparent"
)

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
#' course: that relationships can matter. Now let's work through the functions
#' of the `igraph` library to build-up networks like these as well as those used
#' in the later exercises.
#'
#' @aside The [network_quickplot()] function is what you will use throughout these
#' practicals to visualize networks. This function streamlines supplying arguments
#' to a several [ggplot2] functions and returns the resulting [ggplot2::ggplot]
#' object. You should recall [patchwork_grid()] from previous sessions where you
#' were laying out multiple plots for comparison; we use it here in the same way.

#' @section Basic Structures
#'
#' the `igraph` library has several functions to create common network
#' structures, including both deterministic ones (e.g. connected graphs and
#' lattices) and probabilistic generators (e.g. Erdos-Renyi random graph).
#'
#' In `igraph`, the general convention is that the deterministic functions start
#' with `make_...`, and the probabilistic generators start with `sample_...`

# make several different kinds of graphs, at two sizes
igL <- make_full_graph(n = 9) |> add_layout_(with_graphopt())
igB <- make_full_graph(n = 25) |> add_layout_(with_graphopt())
igLgnp <- sample_gnp(n = 9, p = 0.2) |> set_graph_attr("layout", igL$layout)
igBgnp <- sample_gnp(n = 25, p = 0.2) |> set_graph_attr("layout", igB$layout)
igLl <- make_lattice(length = 3, dim = 2) |> add_layout_(on_grid())
igBl <- make_lattice(length = 5, dim = 2) |> add_layout_(on_grid())

# plot them together as a grid
list(list(
  "N=9 Cluster"  = network_quickplot(igL, simple = TRUE),
  "N=25 Cluster" = network_quickplot(igB, simple = TRUE)
), list(
  "N=9 Random"  = network_quickplot(igLgnp, simple = TRUE),
  "N=25 Random" = network_quickplot(igBgnp, simple = TRUE)
), list(
  "N=9 Lattice"  = network_quickplot(igLl, simple = TRUE),
  "N=25 Lattice" = network_quickplot(igBl, simple = TRUE)
)) |> patchwork_grid()

#' @question How would you describe the difference between the graphs created by
#' `make_full_graph()` vs. `sample_gnp()` vs. `make_lattice()`?
#' @answer The `make_...` generators produce graphs with fixed properties; all
#' vertices connected for `make_full_graph()` and vertices connected in a grid
#' for `make_lattice()` (a typical 2D one for the basic arguments, or more
#' complicated grids if you fiddled around with `dim=` arguments).
#'
#' @question Of the three generators, `make_full_graph()`, `make_lattice()`, and
#' `sample_gnp()`, which do you think is behind the plots we first looked at?
#' @answer The example networks above are made with `make_lattice()`, with some
#' other modifications. In the later exercises, we will use `make_full_graph()`.
#'
#' @hint look at `?make_full_graph`, `?make_lattice`, and `?sample_gnp`.
#'
#' @aside Note that we are adding layouts to the plots above. We do that to
#' ensure getting particular plots; without, each plotting of an igraph object
#' gives different arrangements of vertices and edges.

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

list(list(
  "N=25 Lattice" = network_quickplot(igBl, simple = TRUE),
  "N=25 Lattice--" = network_quickplot(igBlmod, simple = TRUE)
)) |> patchwork_grid() & geom_text(
  aes(x=(x1+x2)/2, y=(y1+y2)/2, label = eid), data = network_edge_data(igBl |> as.data.table())
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
  aes(x=(x1+x2)/2, y=(y1+y2)/2, label = eid), data = network_edge_data(igBBl |> as.data.table())
)

#' In a later section, we'll use some of the other [igraph] capabilities to
#' do this more cleverly. For now, let's consider the tools to add vertices
#' and edges to a graph.

# adding edges ...
iglstar <- make_star(9, mode = "undirected") |> add_layout_(as_star())
# can be done by naming specific vertices
iglstarmod <- iglstar |> add_edges(c(c(2,4), c(3,5), c(6,8), c(7,9)))

# TODO fix labels
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

#' Now let's use the approach of adding on another graph.
# TODO: add whole other graphs

# TODO: recycle below


#' ASIDE: It's also commonly useful to build up graphs
#' by merging building blocks
#' TODO some aside code


#' @section Modifying Networks, part 2
#'
#' The previous section introduced directly modifying networks: adding and/or
#' deleting edges and vertices. We *could* use this approach to make the
#' networks from the top of this practical.
#'
#' In those networks, we have some _vaccinated_ and _unvaccinated_
#' individuals, and the edges (over which transmission occurs) connecting
#' vaccinated individuals do not appear - but not because they have been deleted!
#' The model here is not that these people no longer contact the unvaccinated
#' population, simply that transmission does not occur along those contacts.
#'
#' We can create plots (and more importantly, simulations) that maintain these
#' interactions while *also* capturing that this transmission is route is
#' blocked. To do so, we typically modify the attributes of edges and vertices,
#' often based on the attributes their connected edges and vertices.
#'
#' TODO: merge this
#' We frequently use network models to capture detailed structural
#' relationships between elements. However, we still need to model
#' the infectious disease phenomena. In `igraph`, we can assign
#' both vertices and edges essentially as many attributes as we
#' can imagine.
#'
#' There are also the `V(g)[...]` and `E(g)[...]` functions for
#' accessing the `V`ertices and `E`dges that match the properties
#' in the `[...]` block (much like using `which(...)` or `subset(...)`
#' from base R)

#' TODO: switch this to be about vaccinated and unvaccinated
#' assign vertex properties; in this example, give the population
#' the Susceptible status, then change one individual to the
#' Infectious status
V(ig10)$state <- "S"
V(ig10)[1]$state <- "I"

#' Q: Using the `ig30` network, set about 10% of the population to
#' "I" and check your results by plotting them as above. Try this a
#' few times.

V(ig30)$state <- "S"
V(ig30)[`???`]$state <- "I" # hint: ?runif, ?vcount
V(ig30)$color <- "dodgerblue"
V(ig30)[state == "I"]$color <- "firebrick"
plot(ig30, layout = ig30layout)

#' TODO: @aside this material, highlighting what you need to do in base [igraph]
#' for these visuals
#' there are some special attributes that get used in plotting, e.g.
#' `color`. We can use our model attributes to decide how to set
#' those special attributes if we want to visualize what we've done
V(ig10)$color <- "dodgerblue"
V(ig10)[state == "I"]$color <- "firebrick"
plot(ig10, layout = ig10layout)


#' use vertex and edge properties
#' in particular, we should give them code that will visualize SIR & transmission pathways
... visualize with properties
... lookup / reassign by property

#' Q: for a network with the property state SIR, how could check if that network has any infectious
#' individuals?

#' vertex and edge selection based on indices / typical true/false selection
#' also with `sample`?

#' Q: how could you randomly sample vertices that are ...?

#' vertex and edge selection based on network properties
...self-selection
%--%
.inc, etc

#' Q: how could you select all edges next to an infectious individual
#' Q: how could you select all vertices at the end of an edge where the other end
#' is infectious?

#' @section Follow Up Work
#'
#' We only touched the surface of the capabilities of the [igraph] library.
#' There are many other network generation functions (try `igraph::make_` or
#' `igraph::sample_` and then TAB to see suggestions for others), as well as
#' functions to save and load networks in various formats (TODO: examples).
#'
#' [igraph] also has a wealth of analytical tools for computing various metrics.
#' We do not use those in this session, but they are often part of evaluations
#' to try to understand why simulations on different networks have different
#' features, or to predict what kind of epidemic outcomes will result from
#' different network properties.

#' TODO from practical 3 demo - what network property calculations
#' do we use? have them use some of the igraph network metric calculations

#' advanced stuff:
#' edge lists, adjaceny lists,
#' reading in / writing out
#' generators e.g. erdos-renyi














###############################################################################
#                     Introduction to iGraph: Practical 0                     #
###############################################################################

while (!require(igraph)) install.packages("igraph")

########## 1 - Creating iGraph objects  #######################################
# igraph is built around working with igraph objects, which are typically built
# from "edgelists" or "adjacency matrices".
#
# An edge list is a N x 2 matrix, where each row-pair corresponds to an edge
?graph_from_edgelist

# make an edge list - we're (r=row)binding together a series of vertex pairs
el <- rbind(c(1,2),c(1,3),c(2,3),c(3,4))
# use it to make an igraph object
ig <- graph_from_edgelist(el, directed = FALSE)
# view that object
plot(ig)

# a) What happens if you change the order of the elements in the assignment
# of `el` - i.e. swap c(1,2) and c(1,3)?  Or change c(1,2) to c(2,1)?
# Note, the graph will almost certainly *look* different; check the
# the edges and vertices in detail.
# Answer:

el <- #### <YOUR CODE HERE> ####
ig <- graph_from_edgelist(el, directed = FALSE)
plot(ig)

# b) Change something about the pairs in `el` such that there will be a
# meaningful change when you `plot(ig)`
# Answer:

el <- #### <YOUR CODE HERE> ####
ig <- graph_from_edgelist(el, directed = FALSE)
plot(ig)

# While an edge provides a series of this vertex->that vertex, an adjacency matrix
# has rows and columns correspond to vertices, and then 0 or 1 entry corresponding abscence
# or presence of an edge

# to make the same plot we made earlier...
# construct an empty matrix
al <- matrix(0, nrow=4, ncol = 4)
# connect the same vertices
al[1,2] <- al[1,3] <- al[2,3] <- al[3,4] <- 1
ig <- graph_from_adjacency_matrix(al, mode = "undirected")
plot(ig)
# advanced aside: we can also use slice assignment here:
# el <- rbind(c(1,2),c(1,3),c(2,3),c(3,4))
# al[el] <- 1 # the same as al[1,2] <- al[1,3] <- ... <- 1

# c) Create `al` such that it produces the same plot as the different `el` you created
# for (b).
# Answer:
al <- matrix(0, nrow=4, ncol = 4)
#### <YOUR CODE HERE> #### e.g., al[...] <- 1
ig <- graph_from_adjacency_matrix(al, mode = "undirected")
plot(ig)


# Advanced aside: you may have noticed the arguments `directed = FALSE` and
# `mode = "undirected`.  We have only talked about undirected graphs so far,
# but directed graphs can also be useful. In a directed graph,
# edges go *from* one vertex *to* another.  Feel free to try making them now,
# but for the rest of the exercises we will only be using *undirected* graphs

# directed options
el <- rbind(c(1,2),c(1,3),c(2,3),c(3,4))
ig <- graph_from_edgelist(el)
plot(ig)
al <- matrix(0, nrow=4, ncol = 4)
al[el] <- 1
ig <- graph_from_adjacency_matrix(al)
plot(ig)

# You can of course store and read in graphs in various formats
write_graph(ig, file = "example.csv", format = "edgelist")
# though pay attention to the directed-ness when reading in from edgelists
ig2 <- read_graph(file = "example.csv", format = "edgelist")
ig3 <- read_graph(file = "example.csv", format = "edgelist", directed = FALSE)
# d) Try out the different formats, so you can recognize which is which.
# Describe the differences.
# Answer:



########## 2 - Better ways of Creating iGraph objects  ########################
# igraph also provides constructors for archetypal graphs:
ig.star <- make_star(5, mode = "undirected")
plot(ig.star)
ig.ring <- make_ring(5)
plot(ig.ring)

# and you can do things like add those together:
ig.combo <- ig.star + ig.ring

# a) What do you expect `ig.combo` looks like plotted?
# Answer:



# Let's try it:
plot(ig.combo)

# that plot is probably one of (1) exactly what you expected and (2) the opposite
# To get the other (whichever it was for you), you can do something like:
ig.combo <- graph.union(ig.star, ig.ring)
plot(ig.combo)

# b) Try some of the other `make_...` functions.
# What are `make_lattice`, `make_full_graph`, `make_tree` graphs like?
# Which one do you think will be relevant to the Reed-Frost SIR model?
# Answer:



# Finally, you can create new graphs using `-` and `+` operators;
# note that these operations do *not* change the underlying graph
# unless you reassign the results
ig.combo - edge("1|4")
plot(ig.combo)
ig.combo <- ig.combo - edge("1|4")
plot(ig.combo)

########## 3 - Manipulating iGraph objects  ###################################
# igraph also a variety of ways to interact with edges and vertices

ig.ref <- make_tree(20, mode = "undirected")
plot(ig.ref)

# igraph provides two useful ways to interact with edges and vertices:
V(ig.ref)
E(ig.ref)

# a) How many vertices are there? How many edges?
# Answer:



# You can also use `E` and `V` to assign properties
E(ig.ref)$color <- c("red", "green")
V(ig.ref)$color <- "blue"
plot(ig.ref)
V(ig.ref)$color[c(5,10,15,20)] <- "purple"
plot(ig.ref)

# There are several specific properties associated with plotting; `?plot.igraph`
# for more detail.  But these properties can be anything, such as vaccine status:

ig.grid <- make_lattice(length=10, dim=2)
V(ig.grid)$color <- "yellow"
plot(ig.grid)
V(ig.grid)$vax_status <- "unvaccinated"
V(ig.grid)$vax_status[seq(1,100,by=11)] <- "vaccinated"

# and those properties can be used to filter the graph for other purposes:

V(ig.grid)[vax_status == "vaccinated"]$color <- "blue"

# b) The previous `plot(ig.grid)` appeared as a yellow square; what will
# setting the 1, 12, 23, ..., 100 vertices to blue do?
# Answer:

# Check your answer
plot(ig.grid)

# We can use a similar approach for edge properties:
E(ig.grid)$transmissible <- TRUE # ...initially make all edges transmissible
# there are special functions for selecting edges based on vertices and
# vice versa; use `.inc` (one such function) to make the connections to vaccinated individuals
# non-transmissible
E(ig.grid)[.inc( # get incident edges to...
  V(ig.grid)[vax_status == "vaccinated"] # all vertices with a particular vax_status
)]$transmissible <- FALSE # and set their property `transmissible` to FALSE

# c) you will probably need to use these special functions in the next
# practicals.  Read ?`igraph-es-indexing` and ?`igraph-vs-indexing`.
# List the special filtering functions with a short description of what
# they do.
# Answer:


# Read these outputs to see how `ig.grid` has changed
E(ig.grid)$transmissible
E(ig.grid)[transmissible == FALSE]
V(ig.grid)[vax_status == "vaccinated"]
V(ig.grid)[vax_status == "unvaccinated"]

# Let's use these properties to show how our model targetted vaccine
# campaign has partitioned the network
E(ig.grid)$color <- "red"
E(ig.grid)[transmissible == FALSE]$color <- "blue"
plot(ig.grid)

# recalling to earlier in this practical, we could also simply delete edges
# based on these properties:
plot(ig.grid - E(ig.grid)[transmissible == FALSE])
