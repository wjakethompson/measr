aic <- function(model) {
  model <- check_model(model, required_class = "measrfit", name = "model")
  log_lik <- model$model$value

  num_params <- model$model$par |>
    tibble::as_tibble() |>
    dplyr::mutate(param = names(model$model$par)) |>
    dplyr::filter(!grepl("pi", .data$param),
                  !grepl("log_Vc", .data$param)) |>
    nrow() - 1

  aic <- (-2 * log_lik) + (2 * num_params)

  return(aic)
}

bic <- function(model) {
  model <- check_model(model, required_class = "measrfit", name = "model")
  log_lik <- model$model$value

  num_params <- model$model$par |>
    tibble::as_tibble() |>
    dplyr::mutate(param = names(model$model$par)) |>
    dplyr::filter(!grepl("pi", .data$param),
                  !grepl("log_Vc", .data$param)) |>
    nrow() - 1

  n <- model$data$data |>
    dplyr::distinct(.data$resp_id) |>
    nrow()

  bic <- (-2 * log_lik) + (log(n) * num_params)

  return(bic)
}
