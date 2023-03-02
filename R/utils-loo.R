prep_loglik_array <- function(model) {
  UseMethod("prep_loglik_array")
}

#' @export
prep_loglik_array.measrdcm <- function(model) {
  score_data <- model$data$data
  clean_qmatrix <- model$data$qmatrix %>%
    dplyr::select(-"item_id") %>%
    dplyr::rename_with(~glue::glue("att{1:(ncol(model$data$qmatrix) - 1)}"))
  stan_data <- create_stan_data(dat = score_data, qmat = clean_qmatrix,
                                type = model$type)
  stan_draws <- if (model$method == "mcmc") {
    get_mcmc_draws(model)
  } else if (model$method == "optim") {
    get_optim_draws(model)
  }

  stan_pars <- create_stan_gqs_params(backend = model$backend,
                                      draws = stan_draws)
  stan_pars$data <- stan_data

  # compile model -----
  stan_mod <- create_stan_function(backend = model$backend,
                                   method = "gqs",
                                   code = loglik_script(),
                                   pars = stan_pars,
                                   silent = 2)
  out <- utils::capture.output( #nolint
    gqs_model <- do.call(stan_mod$func, stan_mod$pars)
  )

  log_lik_array <- if (model$backend == "rstan") {
    posterior::as_draws_array(as.array(gqs_model, pars = "log_lik"))
  } else if (model$backend == "cmdstanr") {
    gqs_model$draws(variables = "log_lik", format = "draws_array")
  }

  return(log_lik_array)
}
