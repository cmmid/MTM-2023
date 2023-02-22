

# Solve an ODE equation 
# Arguments are the parameter values
solveODE_2 <- function(parameters){
  
  # Define time to solve equations
  times <- seq(from = 0, to = 50, by = 1)
  
  # Define initial conditions
  N <- 100
  I_0 <- 1
  S_0 <- N - I_0
  R_0 <- 0
  state <- c( S = S_0, I = I_0, R = R_0)
  
  # Solve equations
  output_raw <- ode(y = state, times = times, func = SIR_model_2, parms = parameters,
                    method = "rk4")
  # Convert to data frame for easy extraction of columns
  output <- as.data.frame(output_raw)
  return(max(output[,"I"]))
}

### Define the SIR model
SIR_model_2 <- function(times, state, parms){
  ## Define variables
  S <- state["S"]
  I <- state["I"]
  R <- state["R"]
  N <- S + I + R
  
  # Extract parameters
  R0 = parms["R0"]
  gamma <- parms["gamma"]
  beta = gamma * R0
  
  # Define differential equations
  dS <- - (beta * S * I) / N
  dI <- (beta * S * I) / N - gamma * I
  dR <- gamma * I
  res <- list(c(dS, dI, dR))
  return(res)
}

# function to plot the output of a monte carlo sampling analysis
# input.df is a matrix, where by default the 
# - first column is the parameter being sampled randomly
# - second column is the parameter being changed discretely (not sampled)
# - third column is the output parameter or variable of interest
MCplot <- function(input.df, MC.param = 1, discrete.param = 2, output.param = 3){
  
  # delete all the rows where there is a NA for the output
  input.df <- input.df[!is.na(input.df[,output.param]),]
  # grab the maximum value needed to plot
  max.max <- max(input.df[,output.param], nan.ignore=TRUE)
  
  # draw an empty plotting window with the discrete parameter on the x axis
  plot(NULL, xlab = sprintf("%s", names(input.df)[discrete.param]), 
       ylab = sprintf("%s", names(input.df)[output.param]), 
       xlim = c(min(input.df[,discrete.param]), max(input.df[,discrete.param])), 
       ylim = c(0,max.max))
  par(new=TRUE)
  
  # add in the individuals points
  for (i in unique(input.df[,discrete.param])){
    # for each unique value in the discrete parameter sequence
    mc.vals <- input.df[input.df[,discrete.param]==i,]
    
    for (j in 1:dim(mc.vals)[1]){
        points(i, mc.vals[j,output.param])
    }
  }
  
  title(sprintf("Monte Carlo Sampling over %s", names(input.df)[MC.param]))
}