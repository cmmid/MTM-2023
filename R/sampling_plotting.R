
#' @title Plot Monte Carlo Results
#'
#' @description
#' Produces a plot of Monte Carlo Simulation Results for SIR model
#'
#' @param data a `data.frame`, with columns `R0`, `gamma`, and `max.prev`
#'
#' @return a [ggplot2::ggplot()] object
#'
#' @export
sampling_MC_plot <- function(data) {
  return(ggplot(data = data) +
    geom_violin(aes(x = factor(1/gamma), y = max.prev)) +
    xlab("Infectious duration (days)") +
    ylab("Maximum prevalence") +
    title("Monte Carlo Sampling over R0")
  )
}
