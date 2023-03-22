#' @importFrom posterior as_draws
#' @export
posterior::as_draws

#' @export
as_draws.measrfit <- function(x, ...) { #nolint
  if (x$backend == "rstan" && x$method == "optim") {
    posterior::as_draws(t(as.matrix(x$model$par)))
  } else {
    posterior::as_draws(x$model, ...)
  }
}
