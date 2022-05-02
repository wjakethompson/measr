#' @importFrom tidybayes tidy_draws
#' @export
tidybayes::tidy_draws

#' @export
tidy_draws.measrfit <- function(model, ...) {
  tidybayes::tidy_draws(model$model, ...)
}
