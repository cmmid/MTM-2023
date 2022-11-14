
require(MTM)
require(igraph)

reminder(
"This material is written using the `igraph` library.
There are other libraries that provide the same basic
functionality, but via different approaches, e.g. `networkx`"
)

#' @section Motivation
#'
#' Network-based models (aka, graphs) are useful when the system you are trying
#' to capture has lots of relationship structure that needs to be represented
#' to accurately reproduce what is going on.
#'
#' In this warmup, we're going to show you how to build up these two networks:

network_warmup_vaccine_random # random vaccination in a small population
network_warmup_vaccine_ordered # targetted vaccination

#' @section Basic Structures
#'
#' the `igraph` library has several functions to create common
#' network structures, including both deterministic ones
#' (e.g. fully connected graphs) and probabilistic generators
#' (e.g. Erdos-Renyi random graph)
#'
#' In `igraph`, the general convention is that the deterministic
#' functions start `make_...`, and the probabilistic generators
#' start `sample_...`

ig10 <- make_full_graph(n = 10)
ig30 <- make_full_graph(n = 30)
ig10layout <- layout_with_graphopt(ig10)
ig30layout <- layout_with_graphopt(ig30)
plot(ig10, layout = ig10layout); print(ig10)
plot(ig30, layout = ig30layout); print(ig30)

ig10gnp <- sample_gnp(10, 0.2)
ig30gnp <- sample_gnp(30, 0.2)
plot(ig10gnp, layout = ig10layout); print(ig10gnp)
plot(ig30gnp, layout = ig30layout); print(ig30gnp)

#' @question How would you distinguish the graphs created by `make_full_graph()`
#' versus `sample_gnp()`?
#' @answer
#' @hint reading `?make_full_graph` and `?sample_gnp` might be helpful.

#' @aside Generally, each visualisation an igraph object gives different layouts
#' To get the same plot, you can generate a layout for a graph (as we do above).
#' Layouts can be useful for comparing across different graphs as well
plot(ig10); plot(ig10) # compare these two vs the next two
plot(ig10, layout = ig10layout); plot(ig10, layout = ig10layout)
plot(ig10, layout = ig10layout); plot(ig10gnp, layout = ig10layout)


##################### PART B ################################

#' The generation functions are useful building blocks, but
#' are not typically sufficient to get networks that exactly
#' match our models. For that, there are tools to add/delete
#' edges and vertices, to merge graphs, and so on.
#'
#' We will show some highlights here.

#' delete every other edge
ig10mod <- delete_edges(ig10, 1:(ecount(ig10)/2)*2)
plot(ig10, layout = ig10layout); plot(ig10mod, layout = ig10layout)

#' add edges to a star graph
ig10star <- make_star(10, mode = "undirected")
ig10starmod <- add_edges(ig10star, c(c(2,4), c(3,5), c(6,8), c(7,9)))
plot(ig10star, layout = ig10layout)
plot(ig10starmod, layout = ig10layout)

#' add vertices
ig30starp <- add_vertices(ig10star, 20)
plot(ig30starp, layout = ig30layout)
#' note: adding vertices doesn't also add edges

#' Q: Add edges to `ig30starp` to make it a star graph.
#' Check your work by plotting the result compared to
#' making a 30 star automatically:
ig30star <- add_edges(ig30starp, `???`)

plot(ig30star, layout = ig30layout)
plot(make_star(30, mode = "undirected"), layout = ig30layout)

#' ASIDE: It's also commonly useful to build up graphs
#' by merging building blocks
#' TODO some aside code


##################### PART C ################################

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

#' assign vertex properties; in this example, give the population
#' the Susceptible status, then change one individual to the
#' Infectious status
V(ig10)$state <- "S"
V(ig10)[1]$state <- "I"

#' there are some special attributes that get used in plotting, e.g.
#' `color`. We can use our model attributes to decide how to set
#' those special attributes if we want to visualize what we've done
V(ig10)$color <- "dodgerblue"
V(ig10)[state == "I"]$color <- "firebrick"
plot(ig10, layout = ig10layout)

#' Q: Using the `ig30` network, set about 10% of the population to
#' "I" and check your results by plotting them as above. Try this a
#' few times.

V(ig30)$state <- "S"
V(ig30)[`???`]$state <- "I" # hint: ?runif, ?vcount
V(ig30)$color <- "dodgerblue"
V(ig30)[state == "I"]$color <- "firebrick"
plot(ig30, layout = ig30layout)

#'

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
