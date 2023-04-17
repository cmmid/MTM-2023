## code to prepare `network_warmup_vaccine_ordered` dataset goes here

require(igraph)

ref <- make_lattice(length = 10, dim = 2) |>
  add_layout_(on_grid())

modgraph <- function(ig, vaccinees) {
  V(ig)$state <- "unvaccinated"
  V(ig)$state[vaccinees] <- "vaccinated"

  # use vaccine state to split
  E(ig)$state <- "transmissible"
  E(ig)[
    .inc(V(ig)[state == "vaccinated"])
  ]$state <- "blocked"
  ig
}

set.seed(8675309)

orderedvaccinees <- seq(1, 100, by = 11)
randomvaccinees <- sample(100, length(orderedvaccinees), replace = FALSE)

# set up vaccinees such that they split the graph

network_warmup_vaccine_ordered <- modgraph(ref, orderedvaccinees)
network_warmup_vaccine_random <- modgraph(ref, randomvaccinees)

usethis::use_data(
  network_warmup_vaccine_ordered,
  network_warmup_vaccine_random,
  overwrite = TRUE
)

require(MTM)

# sample a few hundred networks
network_unstructured_set <- lapply(
  1:300, function(sample_id) {
    set.seed(sample_id)
    list(N = 50, p = 0.04) |> network_percolate()
  }
)

keep_edges <- function(og, keepn) {
  delete_edges(og, sample(E(og), ecount(og) - keepn))
}

structure_network <- function(usn) {
  en <- ecount(usn)
  one <- rbinom(1, en - 1, 0.5)
  two <- en - 1 - one
  og <- make_full_graph(ceiling(vcount(usn) / 2)) |> keep_edges(one)
  tg <- make_full_graph(floor(vcount(usn) / 2)) |> keep_edges(two)
  cg <- og + tg + edge(
    sample(vcount(og), 1),
    vcount(og) + sample(vcount(tg), 1)
  )
  V(cg)$state <- V(usn)$state
  E(cg)$draw <- E(usn)$draw
  E(cg)$state <- E(usn)$state
  cg$states <- usn$states
  cg$inf_states <- usn$inf_states
  cg$layout <- usn$layout
  return(cg)
}

network_structured_set <- network_unstructured_set |> lapply(structure_network)

usethis::use_data(
  network_unstructured_set,
  network_structured_set,
  overwrite = TRUE
)

#' @examples
#' tmp.ig <- network_warmup_vaccine_ordered
#' E(tmp.ig)$color <- "grey"
#' E(tmp.ig)[state == "blocked"]$color <- NA
#'
#' plot(tmp.ig)
#' tmp.ig <- network_warmup_vaccine_random
#' E(tmp.ig)$color <- "grey"
#' E(tmp.ig)[state == "blocked"]$color <- NA
#'
#' plot(tmp.ig)
