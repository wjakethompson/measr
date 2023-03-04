#' @importFrom posterior as_draws
#' @export
posterior::as_draws

#' @export
as_draws.measrfit <- function(x, ...) { #nolint
  posterior::as_draws(x$model, ...)
}
