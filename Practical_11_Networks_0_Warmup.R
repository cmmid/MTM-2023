
#' must have features:

#' making some different core graphs
#' plot these different canned graphs
make_full_graph(N, directed = FALSE)
...

#' Q: how would you describe the graphs these functions create?

#' modifying graphs
delete_edges etc

#' Q: perform modication X, check your work by plotting the result

#' assign vertex and edge properties
V(ig)$state <- "whatever"
E(ig)$state <- "whatever"

#' Q: assign some property X, confirm that you've done so with the following command

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
