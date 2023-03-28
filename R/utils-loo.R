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
  stan_draws <- get_mcmc_draws(model)

  stan_pars <- create_stan_gqs_params(backend = model$backend,
                                      draws = stan_draws)
  stan_pars$data <- stan_data

  # compile model -----
  stan_mod <- create_stan_function(backend = model$backend,
                                   method = "gqs",
                                   code = loglik_script(),
                                   precompiled = stanmodels$gqs_loglik,
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

list_names <- function(x, ...) {
  names <- substitute(list(x, ...), env = parent.frame())[-1]
  names <- unlist(lapply(names, paste_deparse), recursive = TRUE,
                  use.names = TRUE)

  return(names)
}

paste_deparse <- function(x) {
  paste(deparse(x), sep = "", collapse = "")
}
