#' @importFrom posterior as_draws
#' @export
posterior::as_draws

#' @export
`as_draws.measr::measrdcm` <- function(x, ...) {
  if (S7::S7_inherits(x@backend, rstan) && S7::S7_inherits(x@method, optim)) {
    return(posterior::as_draws(t(as.matrix(x@model$par))))
  }

  posterior::as_draws(x@model, ...)
}
