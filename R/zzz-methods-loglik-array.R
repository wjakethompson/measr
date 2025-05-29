#' Extract the log-likelihood of an estimated model
#'
#' The `loglik_array()` methods for [measrdcm][dcm_estimate()] objects
#' calculates the log-likelihood for an estimated model via the generated
#' quantities functionality in *Stan* and returns the draws of the `log_lik`
#' parameter.
#'
#' @param model A [measrdcm][dcm_estimate()] object.
#' @param ... Unused. For future extensions.
#'
#' @return A "[`draws_array`][posterior::draws_array()]" object containing the
#'   log-likelihood estimates for the model.
#' @export
loglik_array <- S7::new_generic("loglik_array", "model")


S7::method(loglik_array, measrdcm) <- function(model) {
  stan_data <- stan_data(model)
  stan_draws <- get_draws(model, vars = c("log_Vc", "pi"))
  stan_args <- default_stan_args(model@backend, gqs(), draws = stan_draws)
  stan_args$data <- stan_data

  stan_function_call <- stan_call(
    backend = model@backend,
    method = gqs(),
    code = dcmstan::stan_code(dcmstan::generated_quantities(loglik = TRUE)),
    args = stan_args,
    precompiled = stanmodels$gqs_loglik
  )
  out <- capture.output( #nolint
    mod <- do.call(stan_function_call$call_function,
                   stan_function_call$args)
  )

  extract_stan_draws(backend = model@backend, method = gqs(),
                     model = mod, vars = "log_lik")
}



loglik <- S7::new_generic("loglik", "model")

S7::method(loglik, measrdcm) <- function(model) {
  calc_loglik(model@backend, model@method, model = model)
}

calc_loglik <- S7::new_generic("calc_loglik", c("backend", "method"),
  function(backend, method, model) {
    S7::S7_dispatch()
  })

S7::method(calc_loglik, list(stanbackend, mcmc)) <- function(backend, method, model) {
  log_lik <- loglik_array(model)
  sum(apply(log_lik, c(3), mean))
}

S7::method(calc_loglik, list(rstan, optim)) <- function(backend, method, model) {
  model@model$value
}

S7::method(calc_loglik, list(cmdstanr, optim)) <- function(backend, method, model) {
  model@model$lp()
}
