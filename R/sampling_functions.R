
#' @title SIR Step Functions
#' @description
#' Provides a step function for a *S*usceptible-*I*nfectious-*R*emoved
#' ODE model, using a beta-gamma parameterization, complying with the
#' [deSolve::ode()] `func` argument requirements.
#'
#' @param t time derivatives to be evaluated (ignored in this model)
#'
#' @param state a named, numeric vector, c(S=.., I=..., R=...); the current
#' system state
#'
#' @param parms a list of parameters, containing `beta` and `gamma` values
#' (for `sampling_dSIR_betagamma`) or `gamma` and `R0` values
#' (for `sampling_dSIR_gammaR0`).
#' All parameters must be positive, scalar values.
#'
#' @return a list, complying with the [deSolve::ode()] `func` return
#' requirement
#'
#' @examples
#' require(MTM)
#' require(deSolve)
#' pars <- list(beta = 0.4, gamma = 0.2, R0 = 2)
#' bg <- ode(
#'   y = c(S=99, I=1, R=0), times = 0:50,
#'   func = sampling_dSIR_betagamma, parms = pars, method = "rk4"
#' )
#' gR <- ode(
#'   y = c(S=99, I=1, R=0), times = 0:50,
#'   func = sampling_dSIR_gammaR0, parms = pars, method = "rk4"
#' )
#' @rdname sampling_models
#' @export
#' @family sampling
sampling_dSIR_betagamma <- function(t, state, parms) {
  with(parms, {
    # compute total population size
    N <- sum(state)

    # define processes
    infection <- (beta * state["S"] * state["I"]) / N
    recovery <- gamma * state["I"]

    # return according to deSolve::ode requirements:
    # list, first element the derivatives, same order as state
    return(list(
      c(dS = -infection, dI = infection - recovery, dR = recovery)
    ))
  })
}

#' @rdname sampling_models
#' @export
sampling_dSIR_gammaR0 <- function(times, state, parms) {
  return(sampling_dSIR_betagamma(
    times, state, within(parms, expression(beta <- gamma * R0))
  ))
}

#' @title Find Max Prevalence in SIR ODE
#' @description
#' Given appropriate parameter values, solves an SIR model,
#' then finds the maximum "I" value. Uses fixed population
#' of c(S=99, I=1, R=0), and runs out to `t=50`.
#'
#' @param parameters a list of the parameters appropriate to use with `dModel`
#'
#' @param dModel either [sampling_dSIR_betagamma()] or [sampling_dSIR_gammaR0()]
#'
#' @param plot_results plot (to the active device) the "S" and "I" compartments
#'
#' @return the maximum "I" value for the solved ODE, a numeric scalar
#' @examples
#' require(MTM)
#' print(sampling_maxprevalence(ist(beta = 0.4, gamma = 0.2)))
#'
#' @export
#' @family sampling
sampling_maxprevalence <- function(
  parameters, dModel = sampling_dSIR_betagamma,
  plot_results = FALSE
) {

  # Define time to solve equations
  times <- seq(from = 0, to = 50, by = 1)

  # Define initial conditions
  N <- 100
  I_0 <- 1
  S_0 <- N - I_0
  R_0 <- 0
  state <- c(S = S_0, I = I_0, R = R_0)

  # solve equations ...
  output <- ode(
    y = state, times = times, func = dModel, parms = parameters,
    method = "rk4"
  ) |> as.data.table() # ... then convert for easy extraction of columns

  if (plot_results) {
    par(new = TRUE)
    par(mfrow = c(1, 1))
    with(output, {
      plot(time, S, type = "l", col = 4, lwd = 2, ylim = c(0, N),
           xlab = "Time", ylab = "Number of People", main = "")
      lines(time, I, lwd = 2, col = 2, type = "l")
      legend("topright", legend = c("Susceptible", "Infected"),
             lty = rep(1, 2), col = c(4, 2), lwd = 2, bty = "n")
    })
  }

  return(max(output$I))
}
