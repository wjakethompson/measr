#' Maximum likelihood based information criteria
#'
#' Calculate information criteria for diagnostic models not estimated with full
#' Markov chain Monte Carlo (i.e., with `method = "optim"`). Available
#' information include the Akaike information criterion (AIC; Akaike, 1973) and
#' the Bayesian information criterion (BIC; Schwarz, 1978).
#'
#' @param x A [measrdcm][dcm_estimate()] object estimated with
#'   `backend = "optim"`.
#' @param ... Unused.
#'
#' @rdname aic-bic
#' @return The numeric value of the information criterion.
#' @export
#'
#' @references Akaike, H. (1973). Information theory and an extension of the
#'   maximum likelihood principle. In B. N. Petrov & F. Csáki (Eds.),
#'   *Proceedings of the Second International Symposium on Information Theory*
#'   (pp. 267-281). Akademiai Kiado.
#' @references Schwarz, G. (1978). Estimating the dimension of a model.
#'   *The Annals of Statistics, 6*(2), 461–464. \doi{10.1214/aos/1176344136}
#'
#' @examplesIf measr_examples()
#' # example code
#' model_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
#'                           identifier = "item")
#' model <- dcm_estimate(dcm_spec = model_spec, data = dcmdata::mdm_data,
#'                       identifier = "respondent", method = "optim",
#'                       seed = 63277)
#'
#' aic(model)
#'
#' bic(model)
aic <- S7::new_generic("aic", "x", function(x, ..., force = FALSE) {
  S7::S7_dispatch()
})

#' @rdname aic-bic
#' @export
bic <- S7::new_generic("bic", "x", function(x, ..., force = FALSE) {
  S7::S7_dispatch()
})

# methods ----------------------------------------------------------------------
S7::method(aic, measrdcm) <- function(x, force = FALSE) {
  if (!rlang::is_empty(x@criteria$aic) && !force) {
    return(x@criteria$aic)
  }

  if (!S7::S7_inherits(x@method, optim)) {
    cli::cli_abort(
      glue::glue("{{.arg {rlang::caller_arg(x)}}} must be a model estimated ",
                 "with {{.code method = \"optim\"}} to calculate the AIC")
    )
  }

  # START HERE: need to generalize for cmdstanr backend -331.764
  log_lik <- loglik(x@backend, model = x)

  num_params <- get_draws(x) |>
    posterior::subset_draws(variable = c("log_Vc", "pi"), exclude = TRUE) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    dplyr::select(-c(".chain", ".iteration", ".draw")) |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "param", values_to = "value") |>
    nrow() - 1

  (-2 * log_lik) + (2 * num_params)
}

S7::method(bic, measrdcm) <- function(x, force = FALSE) {
  if (!rlang::is_empty(x@criteria$bic) && !force) {
    return(x@criteria$bic)
  }

  if (!S7::S7_inherits(x@method, optim)) {
    cli::cli_abort(
      glue::glue("{{.arg {rlang::caller_arg(x)}}} must be a model estimated ",
                 "with {{.code method = \"optim\"}} to calculate the BIC")
    )
  }

  # START HERE: need to generalize for cmdstanr backend
  log_lik <- x@model$value

  num_params <- get_draws(x) |>
    posterior::subset_draws(variable = c("log_Vc", "pi"), exclude = TRUE) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    dplyr::select(-c(".chain", ".iteration", ".draw")) |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "param", values_to = "value") |>
    nrow() - 1

  n <- length(x@data$respondent_names)

  (-2 * log_lik) + (log(n) * num_params)
}
