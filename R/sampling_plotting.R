
# function to plot the output of a monte carlo sampling analysis
# input.df is a matrix, where by default the
# - first column is the parameter being sampled randomly
# - second column is the parameter being changed discretely (not sampled)
# - third column is the output parameter or variable of interest
#' @title Plot Monte Carlo Results
#' @description
#' Produces a plot of Monte Carlo Simulation Results for SIR model
#'
#' @export
sampling_MC_plot <- function(
  data, MC.param = 1, discrete.param = 2, output.param = 3
){

  # delete all the rows where there is a NA for the output
  data <- data[!is.na(data[,output.param]),]
  # grab the maximum value needed to plot
  max.max <- max(data[,output.param], nan.ignore=TRUE)

  # draw an empty plotting window with the discrete parameter on the x axis
  plot(NULL, xlab = sprintf("%s", names(data)[discrete.param]),
       ylab = sprintf("%s", names(data)[output.param]),
       xlim = c(min(data[,discrete.param]), max(data[,discrete.param])),
       ylim = c(0,max.max))
  par(new=TRUE)

  # add in the individuals points
  for (i in unique(data[,discrete.param])){
    # for each unique value in the discrete parameter sequence
    mc.vals <- data[data[,discrete.param]==i,]

    for (j in 1:dim(mc.vals)[1]){
      points(i, mc.vals[j,output.param])
    }
  }

  title(sprintf("Monte Carlo Sampling over %s", names(data)[MC.param]))
}
