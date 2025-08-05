#' Bayes factor for diagnostic models
#'
#' Calculate Bayes factors for diagnostic models.
#'
#' @param x A [measrdcm][dcm_estimate()] object.
#' @param y A [measrdcm][dcm_estimate()] object.
#' @param ... Unused.
#' @param force If the criterion has already been added to the
#'   model object with [add_criterion()], should it be recalculated. Default is
#'   `FALSE`.
#'
#' @rdname bayes_factor
#' @return A modified [measrdcm][dcm_estimate()] object with the Bayes factor
#'   slot populated with the Bayes factor statistic.
#' @export
#'
#' The results of the Bayes factor calculations are designed to be consistent
#' with the reporting guidelines for Bayesian analysis developedy by Kruschke
#' (2021).
#'
#' @references Kruschke, J. K. (2021). Bayesian analysis reporting guidelines.
#'   *Nature, 5*, 1282-1291. \doi{10.1038/s41562-021-01177-7}
#'
#' @examplesIf measr_examples()
#' # example code
#' dina_spec <- dcmstan::dcm_specify(qmatrix = q_matrix[, -1],
#'                                   measurement_model = dina())
#' dino_spec <- dcmstan::dcm_specify(qmatrix = q_matrix, identifier = "item",
#'                                   measurement_model = dino())
#' rstn_dina <- dcm_estimate(dina_spec, data = dina_data,
#'                           identifier = "resp_id", method = "optim",
#'                           backend = "rstan", seed = 63277,
#'                           precompiled = stanmodels$test_dina)
#' rstn_dino <- dcm_estimate(dino_spec, data = dino_data,
#'                           identifier = "resp_id", method = "optim",
#'                           backend = "rstan", seed = 63277,
#'                           precompiled = stanmodels$test_dina)
#' rstn_dina <- add_respondent_estimates(rstn_dina)
#' rstn_dino <- add_respondent_estimates(rstn_dino)
#'
#' bayes_factor(rstn_dina, rstn_dino)
bayes_factor <- S7::new_generic("bayes_factor", "x",
                                function(x, ..., y, force = FALSE) {
  S7::S7_dispatch()
})

# methods ----------------------------------------------------------------------
S7::method(bayes_factor, measrdcm) <- function(x, y, force = FALSE) {
  if (!rlang::is_empty(x@criteria$bayes_factor) && !force) {
    return(x@criteria$bayes_factor)
  }

  if (rlang::is_empty(x@respondent_estimates)) {
    rlang::abort("error_bad_method",
                 message = glue::glue("Run `add_respondent_estimates()` ",
                                      "before `bayes_factor()`."))
  }

  if ("measr::optim" %in% class(x@method) ||
      "measr::optim" %in% class(y@method)) {
    rlang::abort("error_bad_method",
                 message = glue::glue("Bayes factor is only ",
                                      "available for models estimated with ",
                                      "`method = \"mcmc\"`."))
  }

  if ("measr::cmdstanr" %in% class(x@backend) ||
      "measr::cmdstanr" %in% class(y@backend)) {
    rlang::abort("error_bad_method",
                 message = glue::glue("Bayes factor is only ",
                                      "available for models estimated with ",
                                      "`backend = \"rstan\"`."))
  }

  log_marg_lik1 <- add_marginal_likelihood(x = x)
  log_marg_lik2 <- add_marginal_likelihood(x = y)

  bf <- exp(log_marg_lik1 - log_marg_lik2)

  posterior_probabilities_mod1 <-
    tibble::tibble(model = x@model_spec@measurement_model@model)
  posterior_probabilities_mod2 <-
    tibble::tibble(model = y@model_spec@measurement_model@model)
  for (prior in seq(.1, .9, by = .1)) {
    prior_prob <- c(prior, 1 - prior)

    log_difference <- (log_marg_lik1 + log(prior_prob[1])) -
      (log_marg_lik2 + log(prior_prob[2]))
    posterior_prob_mod1 <- exp(log_difference) / (1 + exp(log_difference))
    posterior_prob <- c(posterior_prob_mod1, 1 - posterior_prob_mod1)
    names(posterior_prob) <- c(x@model_spec@measurement_model@model,
                               y@model_spec@measurement_model@model)
    x_mod <- x@model_spec@measurement_model@model
    y_mod <- y@model_spec@measurement_model@model
    loop_output <- tibble::tibble(prior = prior_prob,
                                  model = names(posterior_prob),
                                  prob = posterior_prob)
    posterior_probabilities_mod1 <-
      dplyr::left_join(posterior_probabilities_mod1,
                       loop_output |>
                         dplyr::filter(.data$model == x_mod) |>
                         dplyr::mutate(prior = paste0("prior_",
                                                      as.character(
                                                        .data$prior
                                                      ),
                                                      "_",
                                                      x_mod)) |>
                         tidyr::pivot_wider(names_from = "prior",
                                            values_from = "prob"),
                       by = "model")
    posterior_probabilities_mod2 <-
      dplyr::left_join(posterior_probabilities_mod2,
                       loop_output |>
                         dplyr::filter(.data$model == y_mod) |>
                         dplyr::mutate(prior = 1 - .data$prior,
                                       prior = paste0("prior_",
                                                      as.character(
                                                        .data$prior
                                                      ),
                                                      "_",
                                                      x_mod)) |>
                         tidyr::pivot_wider(names_from = "prior",
                                            values_from = "prob"),
                       by = "model")
  }

  posterior_probabilities <- dplyr::bind_rows(posterior_probabilities_mod1,
                                              posterior_probabilities_mod2)

  # save Bayes factor to model
  bf_output <- list(bf = bf,
                    comp_model = y_mod,
                    posterior_probability = posterior_probabilities)

  return(bf_output)
}

#' @export
#' @rdname bayes_factor
add_marginal_likelihood <- function(x) {
  if ("<measr::optim>" %in% class(x@method)) {
    rlang::abort("error_bad_method",
                 message = glue::glue("Bayes factor is only ",
                                      "available for models estimated with ",
                                      "`method = \"mcmc\"`."))
  }

  rdcmchecks::check_S7(x, class = "measrfit")

  # calculate log marginal likelihood
  log_marg_lik <- bridgesampling::bridge_sampler(x@model,
                                                 silent = TRUE)

  return(log_marg_lik$logml)
}
