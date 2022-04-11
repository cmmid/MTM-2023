
#' core library
require(igraph)
#' for data manipulation
require(data.table)
#' for plotting
require(ggplot2)
require(patchwork)

update_network <- function(network, delta) {
  changedv <- V(delta)[!is.na(change)]
  V(network)[changedv]$state <- changedv$change
  E(network)$active <- E(delta)$active
  return(network)
}

still_infectious <- function(network) any(V(network)$state == "I")

SIRcolors <- c(S = "dodgerblue", I = "firebrick", R = "forestgreen")

#' the algorithm to simulate Reed-Frost model on a network
#' 
#' @param p, the probability of transmission (a number between 0 and 1)
#' @param current_network, an [igraph] network, the initial state of the population
#' @param deltafun, a function which receives an [igraph] network and `p`, and returns 
#'   a network with vertex attribute `change`, indicating which vertices change,
#'   and edge attribute `active`, indicating which edges contributed to transmission
#' @return a list of [igraph]s, where the list entries correspond to the population
#'   state at time t (e.g. `list[[1]]` is the initial network state after introduction)
#'     
#' This function takes the Reed Frost model parameters (N, p) and implementation
#' of the model (set by setupfun, deltafun). Using those, it performs the model
#' iteration loop, and returns the series networks
#' 
#' @examples
#' # guarantee a network with some transmission
#' set.seed(42)
#' 
#' # using the practical 1 Reed-Frost model definition
#' networkstates <- run_network_reed_frost(
#'   p = 0.1,
#'   network_current = build_network_1(N = 30),
#'   deltafun = next_state_1
#' )
#' 
#' reflayout <- layout_nicely(networkstates[[1]])
#' quickplot <- function(state) plot(
#'   state, layout = reflayout, vertex.color = SIRcolors[V(state)$state],
#'   edge.color = c("grey", "red")[E(state)$active + 1L],
#'   vertex.label = NA, edge.width = 0.5
#' )
#' 
#' quickplot(networkstates[[1]])
#' quickplot(networkstates[[2]])
#' quickplot(networkstates[[3]]) 
#' quickplot(networkstates[[4]]) 
run_network_reed_frost <- function(p, current_network, deltafun) {
  network_storage  <- list(current_network)
  while(still_infectious(current_network)) {
    delta <- deltafun(current_network, p)
    current_network <- update_network(current_network, delta)
    network_storage  <- c(list(current_network), network_storage)
  }
  return(rev(network_storage))
}

flatten_network <- function(network, statelevels = c("S", "I", "R")) {
  setNames(sapply(statelevels, function(s) length(V(network)[state == s])), statelevels)
}

flatten_network_storage <- function(store) {
  s <- sapply(store, flatten_network)
  s <- rbind(t = 1:dim(s)[2], s)
  as.data.table(t(s))
}

sample_reed_frost <- function(n, N, p, setupfun, deltafun) rbindlist(
  lapply(1:n, function(i) {
    set.seed(i);
    return(flatten_network_storage(run_network_reed_frost(p, setupfun(N, p), deltafun)))
  }), idcol = "sample"
)

plot_dur_size <- function(s.dt) {
  #' assert: s.dt is ordered by t, and last entry by sample corresponds to end o
  #' assert: samples is 1:N
  ref.dt <- s.dt[,.(duration=t[.N]-1, final_size = R[.N]), keyby=sample]
  samples <- ref.dt[, sample[.N]]
  
  heat.dt <- ref.dt[ , .N, by=.(duration, final_size) ]
  durahist.dt <- ref.dt[, .N, keyby=.(duration)]
  sizehist.dt <- ref.dt[, .N, keyby=.(final_size)]
  
  max.den <- ceiling(max(durahist.dt$N, sizehist.dt$N)/samples*10)/10
  
  sx <- scale_x_continuous("Duration", breaks = function(ls) seq(0, ls[2], by=4))
  sy <- scale_y_continuous("Final Size", breaks = function(ls) seq(0, ls[2], by=10))
  sdenx <- scale_x_continuous("Density", breaks = function(ls) seq(0, max.den, by=0.1))
  sdeny <- scale_y_continuous("Density", breaks = function(ls) seq(0, max.den, by=0.1))
  geom_dens <- function(fill = "grey60", stat = "identity", width = 1, ...) geom_bar(fill=fill, stat=stat, width=width, ...)
  
  p.heat <- ggplot(heat.dt) + aes(duration, final_size, fill = N/samples) +
    geom_tile() +
    sx + sy +
    coord_cartesian(xlim = c(0, NA), ylim = c(0, NA), expand = FALSE) +
    scale_fill_distiller("Density", palette = "Reds", direction = 1) +
    theme_minimal() +
    theme(legend.position = c(.95, 0.05), legend.justification = c(1,0))
  
  p.dur <- ggplot(durahist.dt) + aes(duration, N/samples) +
    geom_dens() + sx + sdeny +
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    coord_cartesian(xlim=c(0, NA), ylim = c(0, max.den), expand = FALSE)
  
  p.sz <- ggplot(sizehist.dt) + aes(N/samples, final_size) +
    geom_dens(orientation = "y") + sdenx + sy +
    theme_minimal() + theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
    coord_cartesian(xlim=c(0, max.den), ylim = c(0, NA), expand = FALSE)
  
  # Used for testing only, please delete
  # sampler_output <- sampler(num_samples = 1000, simfunc = igraph_sim, observef = rf_observer, n = 50, p = 0.06)
  
  p.tot <- p.dur + plot_spacer() + p.heat + p.sz +
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
  
  return(p.tot)
  
}






require(igraph)
require(ggplot2)
# BELOW IS THE REFERENCE CODE; IF YOU WANT PRACTICALS TO RUN, DON'T MODIFY IT
# Warnings aside: you should examine the code here, as you may find the
# constructs helpful, and the practical scripts apply these functions
# Note: this code is commented using roxygen2 style

#' Pre-allocates an appropriately sized container for Reed-Frost model results
#' 
#' @param n, an integer; the number of individuals in the model
#' @details The maximum number of steps in a Reed-Frost model corresponds to one
#' new infection (no more, no less) each time step + a final time step with that
#' last infectious individual recovering.
#' 
#' @return a n+1 x 3 matrix of `NA_integer_`s with rows labeled by timestep, columns labeled by state.
#'
rf_prealloc <- function(n) matrix(
  NA_integer_,
  nrow = n+1, ncol = 3,
  dimnames = list(
    timestep=0:n,
    state=c("S","I","R")
  )
)

#' Trims the pre-allocated container to only the relevant results
#' 
#' @param result, a ? x 3 matrix, originally constructed w/ \link{rf_prealloc} and
#' subsequently populated with simulation results
#' @return a ? x 3 matrix, trimmed to only the timesteps that matter; final row corresponds to
#' when the outbreak is over
rf_trim <- function(result) result[!is.na(result[,"I"]),]

#' A simple Reed-Frost Model Implementation
#'
#' @return a matrix; each row is the number of S, I, and R at a particular time step
#'         first row is always ${ I=1, S=n-1, R=0 }$
#' @example 
#' cbs <- chainbinom_sim(n=30, p=0.05) # simulate for a population of 30, transmission probabilty = 5%
#' 
chainbinom_sim <- function(n, p, i) {
  set.seed(i)
  state = c(S=n-1,I=1,R=0)
  result <- rf_prealloc(n)
  tm <- 1
  while(state["I"]) {
    result[tm,] <- state # record the state
    pinf <- 1 - (1-p)^state["I"] # determine the probability of infection
    infs <- rbinom(1, state["S"], pinf) # determine the number of new infections
    state["R"] <- state["R"] + state["I"] # update R (all I -> R)
    state["S"] <- state["S"] - infs # update S (infected S -> I)
    state["I"] <- infs # update I (all I -> R, infected S -> I)
    tm <- tm+1 # increment time
  }
  result[tm,] <- state # record the final state
  return(rf_trim(result))
}

# this takes care of sampling w/ a simulator,
# by resetting random number generator, extracting
# results, etc
#' Convenience harness for replicating function runs
#' 
#' @param num_samples, an integer vector; if length > 1, used as sample ids + run seeds. Otherwise
#'   sample ids are 1:num_samples
#' @param simfunc, a function representing the simulation; it will be called with the contents of ...
#' @param observef, a function that measures the simulation, e.g. getting final size
#' @param ..., any arguments to simfunc
#' 
#' @return a matrix, num_samples x 1 + number of columns returned by observef
#' 
sampler <- function(num_samples, simfunc, observef, ...) {
  smp_ids <- if (length(num_samples)==1) 1:num_samples else num_samples
  return(data.frame(t(sapply(smp_ids, function(i) {
    res <- simfunc(..., i=i);
    c(sample=i, observef(res))
  }))))
}

rf_observer <- function(res) c(duration = dim(res)[1]-1, final_size = tail(res,1)[3])


#' plotter
#' 
#' Run, then plot the results of, two different Reed-Frost simulators
#'
#' @param simulator_A one simulator, a function that can receive whatever supplied in ...
#' @param simulator_B another simulator, a function that can receive whatever supplied in ...
#' @param samples, how many replicates for each simulator
#' @param labels, the simulator names, for plotting
#' @return a ggplot object; also prints the plot to current device as a side effect
plotter <- function(simulator_A, simulator_B = chainbinom_sim, samples=100, labels=c(A="Network", B="Chain-Binomial"), ...) {
  resA <- sampler(num_samples=samples, simfunc=simulator_A, observef=rf_observer, ...)
  resB <- sampler(num_samples=samples, simfunc=simulator_B, observef=rf_observer, ...)
  resA$implementation <- labels["A"]
  resB$implementation <- labels["B"]
  plot_data <- rbind(resA, resB)
  return(
    ggplot(plot_data) + aes(x=final_size) +
      facet_grid(implementation ~ .) +
      geom_histogram(binwidth = 1) + theme_minimal()
  )
}

refgraphs <- list.files(path="../graphs", pattern = "*.csv", full.names = T)

#' Plot duration vs. final size
#' 
#' @param sampler_output Output from sampler function: data.frame with three columns (sample, duration and final_size)
#' 
#' @return ggplot plot of duration vs. final size, with univariate distributions
#' 
plot_dur_size <- function(sampler_output) {
  
  # Used for testing only, please delete
  # sampler_output <- sampler(num_samples = 1000, simfunc = igraph_sim, observef = rf_observer, n = 50, p = 0.06)
  
  n_total <- nrow(sampler_output)
  tb_plot <- sampler_output %>%
    group_by(duration, final_size) %>%
    summarise(n = n(),
              .groups = "drop") %>%
    mutate(p = n/n_total) %>%
    group_by(duration) %>%
    mutate(p_duration = sum(n)/n_total) %>%
    ungroup() %>%
    group_by(final_size) %>%
    mutate(p_final_size = sum(n)/n_total) %>%
    ungroup()
  max_density <- max(max(tb_plot$p_duration), max(tb_plot$p_final_size))

  g_main <- tb_plot %>%
    ggplot(aes(x = duration, y = final_size)) +
    geom_tile(aes(fill = n)) +
    scale_x_continuous(limits = c(0, NA),
                       breaks = seq(0, max(sampler_output$duration), 4),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA),
                       breaks = seq(0, max(sampler_output$final_size), 10),
                       expand = c(0, 0)) +
    scale_fill_distiller(palette = "Reds", direction = 1) +
    labs(x = "Duration",
         y = "Final size") +
    theme_bw() +
    theme(legend.position = "none",
          axis.line = element_line(size = 0.5),
          panel.border = element_blank())
  
  g_hist_duration <- sampler_output %>%
    ggplot(aes(x = duration, y = ..density..)) +
    geom_histogram(binwidth = 1, fill = "grey60") +
    scale_x_continuous(limits = c(0, NA),
                       breaks = seq(0, max(sampler_output$duration), 4),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max_density), breaks = seq(0, 1, 0.1)) +
    labs(x = "",
         y = "Density") +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.line = element_line(size = 0.5))
  
  g_hist_size <- sampler_output %>%
    ggplot(aes(y = final_size, x = ..density..)) +
    geom_histogram(binwidth = 1, fill = "grey60") +
    scale_x_continuous(limits = c(0, max_density), breaks = seq(0, 1, 0.1)) +
    scale_y_continuous(limits = c(0, NA),
                       breaks = seq(0, max(sampler_output$final_size), 10),
                       expand = c(0, 0)) +
    labs(x = "Density",
         y = "") +
    theme_classic() +
    theme(axis.text.y = element_blank(),
          axis.line = element_line(size = 0.5))
  
  g_hist_duration + patchwork::plot_spacer() + g_main + g_hist_size +
    patchwork::plot_layout(ncol = 2,
                           nrow = 2,
                           widths = c(4, 1),
                           heights = c(1, 4))
  
  return(out)
  
}
