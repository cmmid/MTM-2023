## code to prepare `network_warmup_vaccine_ordered` dataset goes here

require(igraph)

ref.ig <- make_lattice(length=10, dim=2) |>
  add_layout_(on_grid())

modgraph <- function(src.ig, vaccinees) {
  V(src.ig)$state <- "unvaccinated"
  V(src.ig)$state[vaccinees] <- "vaccinated"

  # use vaccine state to split
  E(src.ig)$state <- "transmissible"
  E(src.ig)[
    .inc(V(src.ig)[state == "vaccinated"])
  ]$state <- "blocked"
  src.ig
}

set.seed(8675309)

orderedvaccinees <- seq(1,100,by=11)
randomvaccinees <- sample(100, length(orderedvaccinees), replace = FALSE)

# set up vaccinees such that they split the graph

network_warmup_vaccine_ordered <- modgraph(ref.ig, orderedvaccinees)
network_warmup_vaccine_random <- modgraph(ref.ig, randomvaccinees)

usethis::use_data(
  network_warmup_vaccine_ordered,
  network_warmup_vaccine_random,
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
