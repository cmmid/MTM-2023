
#' @title Compute 1-step transition for Reed-Frost Chain Binomial Model
#'
#' @param t, the time step; ignored in the Reed-Frost model
#'
#' @inheritParams stochdisc_solve
#'
#' @return an list; first element, a vector of state changes,
#' optional second element, a vector of other global parameter values
#'
#' @seealso stochdisc_solve
#'
#' @examples
#' require(MTM)
#' ps <- list(N=30, p=0.1)
#' sirpop <- c(S = ps$N - 1, I = 1, R = 0)
#' dpop <- stochdisc_dReedFrost(y = sirpop, parms = ps)
#' dpop
#' # ... random results each time
#' stochdisc_dReedFrost(y = sirpop, parms = ps)
#'
#' @export
#' @family stochdisc
stochdisc_dReedFrost <- function(
    t, y, parms, ...
) {
  with(c(as.list(y), parms), {
    # copy y to become dy
    dy <- y
    # probability of any infection == 1 - probability of avoiding all infections
    foi <- 1 - (1 - parms$p)^I
    dI <- rbinom(1, S, foi)
    dy <- c(-dI, dI - I, I)
    return(list(dy))
  })
}

#' @title Solve Stochastic, Discrete Time Models
#'
#' @description Given initial conditions, step function,
#' and parameters, compute the time series of the model.
#'
#' @param y, a vector of initial conditions
#'
#' @param times, a numeric vector, the times to report the state; cast
#' to `0:as.integer(max(times))`. If `NULL` (the default) runs until extinct.
#'
#' @param func, a function that computes the change in state; see
#' [deSolve::ode()] for details of the function signature.
#'
#' @param parms, a list of parameters to pass to `func`.
#'
#' @param ..., ignored
#'
#' @return a [data.table::data.table()] with columns `t` and others
#' corresponding to the state variables in `y`.
#'
#' @examples
#' require(MTM)
#' ps <- list(N=100, p = 0.02)
#' series.dt <- stochdisc_solve(parms = ps)
#' series.dt
#' # ... random results every time
#' series.dt <- stochdisc_solve(parms = ps)
#' series.dt
#'
#' @export
#' @family stochdisc
stochdisc_solve <- function(
    y = c(S = parms$N - 1, I = 1, R = 0),
    times = NULL, func = stochdisc_dReedFrost,
    parms, ...
) {
  yt  <- list(as.list(y))
  t <- 0L
  tmax <- ifelse(
    is.null(times),
    .Machine$integer.max,
    max(as.integer(times))
  )

  dy <- func(t, y, parms, ...)
  # While time remains, and there is at least one state change, compute
  # the distribution at the next time step and store it in yt
  while (
    (t < tmax) && (sum(abs(dy[[1]])) > 0)
  ) {
    t <- t + 1L
    y <- y + dy[[1]]
    yt[[length(yt) + 1]] <- as.list(y)
    dy <- func(t, y, parms, ...)
  }

  return(yt |> data.table::rbindlist(idcol = "t"))
}

#' @title Sample a Discrete Stochastic Simulation
#'
#' @param n an integer; how many samples?
#'
#' @inheritParams stochdisc_solve
#'
#' @param setup_func a function to create new populations
#'
#' @param ref_seed a random seed reference value; each sample run seed is offset
#' from this value
#'
#' @return a [data.table::data.table()], with a `sample` column
#' (`integer`, 1:`n`) & columns from [stochdisc_solve()]
#'
#' @examples
#' require(MTM)
#' ps <- list(N = 100, p = 0.02)
#' samples.dt <- stochdist_sample(100, parms = ps)
#' # final sizes:
#' samples.dt[, .(final_size = R[.N]), by=sample]
#'
#' @export
#' @family stochdisc
stochdisc_sample <- function(
    n, parms,
    func = stochdisc_dReedFrost,
    setup_fun = function(ps) c(S = ps$N - 1, I = 1, R = 0),
    ref_seed = 0
) {
  n |> check_scalar() |> check_natural()

  # for each sample ...
  1L:n |> lapply(function(i) {
    # reset random number seed
    set.seed(i + ref_seed)
    # simulate desired func
    setup_fun(parms) |> stochdisc_solve(func = func, parms = parms)
  }) |> rbindlist(idcol = "sample")
}
