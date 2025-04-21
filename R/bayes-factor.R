bayes_factor <- function(mod1, mod2) {
  if (mod1$method == "optim" || mod2$method == "optim") {
    rlang::abort("error_bad_method",
                 message = glue::glue("Bayes factor is only ",
                                      "available for models estimated with ",
                                      "`method = \"mcmc\"`."))
  }

  mod1 <- check_model(mod1, required_class = "measrfit", name = "model")
  mod2 <- check_model(mod2, required_class = "measrfit", name = "model")

  mod1 <- add_marginal_likelihood(mod1)
  mod2 <- add_marginal_likelihood(mod2)

  log_marg_lik1 <- extract_marginal_likelihood(mod1)
  log_marg_lik2 <- extract_marginal_likelihood(mod2)

  marg_lik1 <- 1 / (1 + exp(-log_marg_lik1))
  marg_lik2 <- 1 / (1 + exp(-log_marg_lik2))

  bf <- marg_lik1 / marg_lik2

  num_params <- model$model$par %>%
    tibble::as_tibble() %>%
    dplyr::mutate(param = names(model$model$par)) %>%
    dplyr::filter(!grepl("pi", .data$param),
                  !grepl("log_Vc", .data$param)) %>%
    nrow() - 1

  aic <- (-2 * log_lik) + (2 * num_params)

  return(aic)
}


# R/loo-methods
extract_log_lik.measrfit <- function(x, parameter_name = "log_lik",
                                     merge_chains = TRUE) {
  if (!inherits(x, "measrfit"))
    stop("Not a measrfit object.", call. = FALSE)
  if (x@mode != 0)
    stop("Stan model does not contain posterior draws.",
         call. = FALSE)
  if (!requireNamespace("rstan", quietly = TRUE))
    stop("Please load the 'rstan' package.", call. = FALSE)
  if (merge_chains) {
    log_lik <- as.matrix(stanfit, pars = parameter_name)
  }
  else {
    log_lik <- as.array(stanfit, pars = parameter_name)
  }
  unname(log_lik)
}

# R/model-evaluation
#' @export
#' @rdname model_evaluation
add_marginal_likelihood <- function(x, overwrite = FALSE, save = TRUE) {
  if (x$method == "optim") {
    rlang::abort("error_bad_method",
                 message = glue::glue("Bayes factor is only ",
                                      "available for models estimated with ",
                                      "`method = \"mcmc\"`."))
  }

  model <- measr:::check_model(x, required_class = "measrfit", name = "x")
  overwrite <- measr:::check_logical(overwrite, name = "overwrite")
  save <- measr:::check_logical(save, name = "force_save")

  # calculate log marginal likelihood
  suppressWarnings(
    log_marg_lik <- bridgesampling::bridge_sampler(samples = model$model,
                                                   maxiter = 10000,
                                                   silent = TRUE)
  )

  # save log marginal likelihood to model
  if (overwrite || is.null(model$log_marginal_likelihood)) {
    model$log_marginal_likelihood <- log_marg_lik$logml
  }

  # re-save model object (if applicable)
  if (!is.null(model$file) && save) {
    saveRDS(model, file = model$file)
  }

  return(model)
}

#' @export
#' @rdname model_evaluation
add_bayes_factor <- function(mod1, mod2, overwrite = FALSE,
                             save = TRUE) {
  if (mod1$method == "optim" || mod2$method == "optim") {
    rlang::abort("error_bad_method",
                 message = glue::glue("Bayes factor is only ",
                                      "available for models estimated with ",
                                      "`method = \"mcmc\"`."))
  }

  # if (!is.null(prior_prob) && length(prior_prob) != 2) {
  #   rlang::abort("error_bad_method",
  #                message = glue::glue("Prior probability must have a length ",
  #                                     "of 2."))
  # }

  # if (!is.null(prior_prob) && sum(prior_prob) != 1) {
  #   rlang::abort("error_bad_method",
  #                message = glue::glue("Prior probabilities do not sum to 1."))
  # }

  mod1 <- add_marginal_likelihood(x = mod1)
  mod2 <- add_marginal_likelihood(x = mod2)

  log_marg_lik1 <- extract_marginal_likelihood(mod1)
  log_marg_lik2 <- extract_marginal_likelihood(mod2)

  bf <- exp(log_marg_lik1 - log_marg_lik2)

  # posterior_probabilities <- tibble::tibble(model = c(mod1$type,
  #                                                     mod2$type))
  posterior_probabilities_mod1 <- tibble::tibble(model = mod1$type)
  posterior_probabilities_mod2 <- tibble::tibble(model = mod2$type)
  for (prior in seq(.1, .9, by = .1)) {
    prior_prob <- c(prior, 1 - prior)

    log_difference <- (log_marg_lik1 + log(prior_prob[1])) -
      (log_marg_lik2 + log(prior_prob[2]))
    posterior_prob_mod1 <- exp(log_difference) / (1 + exp(log_difference))
    posterior_prob <- c(posterior_prob_mod1, 1 - posterior_prob_mod1)
    names(posterior_prob) <- c(mod1$type, mod2$type)
    loop_output <- tibble::tibble(prior = prior_prob,
                                  model = names(posterior_prob),
                                  prob = posterior_prob)#  %>%
    # dplyr::filter(.data$model == mod1$type) %>%
    # dplyr::mutate(prior = paste0("pr_", as.character(.data$prior))) %>%
    # tidyr::pivot_wider(names_from = "prior", values_from = "prob")
    posterior_probabilities_mod1 <- left_join(posterior_probabilities_mod1,
                                              loop_output %>%
                                                dplyr::filter(.data$model ==
                                                                mod1$type) %>%
                                                dplyr::mutate(
                                                  prior =
                                                    paste0("pr_",
                                                           as.character(
                                                             .data$prior
                                                           ))
                                                ) %>%
                                                tidyr::pivot_wider(names_from =
                                                                     "prior",
                                                                   values_from =
                                                                     "prob"),
                                              by = "model")
    posterior_probabilities_mod2 <- left_join(posterior_probabilities_mod2,
                                              loop_output %>%
                                                dplyr::filter(.data$model ==
                                                                mod2$type) %>%
                                                dplyr::mutate(
                                                  prior =
                                                    paste0("pr_",
                                                           as.character(
                                                             .data$prior
                                                           ))
                                                ) %>%
                                                tidyr::pivot_wider(names_from =
                                                                     "prior",
                                                                   values_from =
                                                                     "prob"),
                                              by = "model")

    # tmp <- e^logml*prior_prob
    # post_prob <- as.numeric(e^logml*prior_prob / (tmp[2] + tmp[1]))
    # names(post_prob) <- c(mod1$type, mod2$type)
    # bridge_out <- tibble(prior = prior_prob,
    #                      model = names(post_prob),
    #                      prob = post_prob,
    #                      type = "bridge calc")

    # test_output <- bind_rows(test_output, my_out, bridge_out)

    # testthat::expect_equal(posterior_prob, post_prob)
  }

  posterior_probabilities <- bind_rows(posterior_probabilities_mod1,
                                       posterior_probabilities_mod2)


  # posterior_prob <- tibble::tibble(model = c("m1", "m2"),
  #                                  log_marg_lik = c(log_marg_lik1,
  #                                                   log_marg_lik2),
  #                                  prior = prior_prob) %>%
  #   dplyr::mutate(log_prior = log(.data$prior),
  #                 adj_log_marg_lik = .data$log_marg_lik + .data$log_prior,
  #                 denom = sum(.data$log_marg_lik * .data$prior),
  #                 log_odds_post = .data$adj_log_marg_lik - .data$denom,
  #                 post_prob = exp(.data$log_odds_post) /
  #                   (1 + exp(.data$log_odds_post))) %>%
  #   dplyr::pull(post_prob)

  # posterior_prob <- bridgesampling::post_prob(log_marg_lik1, log_marg_lik2,
  #                                             prior_prob = c(.5, .5),
  #                                             model_names = c(mod1$type,
  #                                                             mod2$type))

  # save Bayes factor to model
  if (overwrite || is.null(mod1$bayes_factor)) {
    mod1$bayes_factor$bf <- bf
    mod1$bayes_factor$comp_model <- mod2$type
    mod1$bayes_factor$posterior_probability <- posterior_probabilities %>%
      dplyr::filter(.data$model == mod1$type)
  }


  # re-save model object (if applicable)
  if (!is.null(mod1$file) && save) {
    saveRDS(mod1, file = mod1$file)
  }

  return(model)
}

# R/utils-extract
extract_marginal_likelihood <- function(model) {
  if (is.null(model$log_marginal_likelihood)) {
    rlang::abort(message = glue::glue("The log_marginal_likelihood ",
                                      "must be added to a model object before ",
                                      "it can be extracted. See ",
                                      "`?add_marginal_likelihood()`."))
  }
  model$log_marginal_likelihood
}

# tests/testthat/setup.R
out <- capture.output(
  suppressMessages(
    rstn_dina <- measr_dcm(data = dina_data, missing = NA, qmatrix = q_matrix,
                           resp_id = "resp_id", item_id = "item", type = "dina",
                           method = "optim", seed = 63277, backend = "rstan",
                           precompiled = stanmodels$test_dina)
  )
)

out <- capture.output(
  suppressMessages(
    rstn_dino2 <- measr_dcm(data = measr:::dino_data, missing = NA, qmatrix = measr:::q_matrix,
                            resp_id = "resp_id", item_id = "item", type = "dino",
                            method = "optim", seed = 63277, backend = "cmdstanr",
                            precompiled = measr:::stanmodels$test_dina)
  )
)

# tests/testthat/test-model-evaluation
test_that("Bayes factor works", {
  rstn_dino <- add_bayes_factor(cmds_mdm_dina, cmds_mdm_lcdm)

  exp_bic <- 37647.64

  expect_equal(rstn_dino$criteria$bic, exp_bic, tolerance = .0001)
})

