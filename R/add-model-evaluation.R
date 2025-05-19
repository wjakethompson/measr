#' Add model evaluation metrics model objects
#'
#' Add model evaluation metrics to fitted model objects. These functions are
#' wrappers around other functions that compute the metrics. The benefit of
#' using these wrappers is that the model evaluation metrics are saved as part
#' of the model object so that time-intensive calculations do not need to be
#' repeated. See Details for specifics.
#'
#' @inheritParams loo::loo
#' @inheritParams dcm2::calc_m2
#' @inheritParams score
#' @param x A [measrdcm][dcm_estimate()] object.
#' @param criterion A vector of information criteria to calculate and add to the
#'   model object. Must be `"loo"` or `"waic"` for models estimated with MCMC,
#'   or `"aic"` or `"bic"` for models estimated with the optimizer.
#' @param method A vector of model fit methods to evaluate and add to the model
#'   object.
#' @param overwrite Logical. Indicates whether specified elements that have
#'   already been added to the estimated model should be overwritten. Default is
#'   `FALSE`.
#' @param save Logical. Only relevant if a file was specified in the
#'   [measrdcm][dcm_estimate()] object passed to `x`. If `TRUE` (the default),
#'   the model is re-saved to the specified file when new criteria are added to
#'   the `R` object. If `FALSE`, the new criteria will be added to the `R`
#'   object, but the saved file will not be updated.
#' @param ... Additional arguments passed relevant methods. See Details.
#' @inheritDotParams fit_ppmc model_fit item_fit
#'
#' @details
#' For `add_respondent_estimates()`, estimated person parameters are added to
#' the `$respondent_estimates` element of the fitted model.
#'
#' For `add_fit()`, model and item fit information are added to the `$fit`
#' element of the fitted model. This function wraps [fit_m2()] to calculate the
#' \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} statistic (Hansen et al., 2016;
#' Liu et al., 2016) and/or [fit_ppmc()] to calculate posterior predictive model
#' checks (Park et al., 2015; Sinharay & Almond, 2007; Sinharay et al., 2006;
#' Thompson, 2019), depending on which methods are specified.
#'
#' For `add_criterion()`, relative fit criteria are added to the `$criteria`
#' element of the fitted model. This function wraps [loo()] or [waic()] to
#' calculate the LOO-CV (Vehtari et al., 2017) or WAIC (Watanabe, 2010),
#' respectively, for models estimated with MCMC.
#' For models estimated with the optimizer, this wraps [aic()] or [bic()]
#' to estimate the AIC (Akaike, 1973) or BIC (Schwarz, 1978), respectively.
#'
#' For `add_reliability()`, reliability information is added to the
#' `$reliability` element of the fitted model. Pattern level reliability is
#' described by Cui et al. (2012). Classification reliability and posterior
#' probability reliability are described by Johnson & Sinharay (2018, 2020),
#' respectively. This function wraps [reliability()]. Arguments supplied to
#' `...` are passed to [reliability()].
#'
#' @return A modified [measrdcm][dcm_estimate()] object with the corresponding
#'   slot populated with the specified information.
#'
#' @references Akaike, H. (1973). Information theory and an extension of the
#'   maximum likelihood principle. In B. N. Petrov & F. Csáki (Eds.),
#'   *Proceedings of the Second International Symposium on Information Theory*
#'   (pp. 267-281). Akademiai Kiado.
#' @references Cui, Y., Gierl, M. J., & Chang, H.-H. (2012). Estimating
#'   classification consistency and accuracy for cognitive diagnostic
#'   assessment. *Journal of Educational Measurement, 49*(1), 19-38.
#'   \doi{10.1111/j.1745-3984.2011.00158.x}
#' @references Hansen, M., Cai, L., Monroe, S., & Li, Z. (2016).
#'   Limited-information goodness-of-fit testing of diagnostic classification
#'   item response models. *British Journal of Mathematical and Statistical
#'   Psychology, 69*(3), 225-252. \doi{10.1111/bmsp.12074}
#' @references Johnson, M. S., & Sinharay, S. (2018). Measures of agreement to
#'   assess attribute-level classification accuracy and consistency for
#'   cognitive diagnostic assessments. *Journal of Educational Measurement,
#'   55*(4), 635-664. \doi{10.1111/jedm.12196}
#' @references Johnson, M. S., & Sinharay, S. (2020). The reliability of the
#'   posterior probability of skill attainment in diagnostic classification
#'   models. *Journal of Educational and Behavioral Statistics, 45*(1), 5-31.
#'   \doi{10.3102/1076998619864550}
#' @references Liu, Y., Tian, W., & Xin, T. (2016). An application of
#'   \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} statistic to evaluate the fit
#'   of cognitive diagnostic models. *Journal of Educational and Behavioral
#'   Statistics, 41*(1), 3-26. \doi{10.3102/1076998615621293}
#' @references Park, J. Y., Johnson, M. S., Lee, Y-S. (2015). Posterior
#'   predictive model checks for cognitive diagnostic models. *International
#'   Journal of Quantitative Research in Education, 2*(3-4), 244-264.
#'   \doi{10.1504/IJQRE.2015.071738}
#' @references Schwarz, G. (1978). Estimating the dimension of a model.
#'   *The Annals of Statistics, 6*(2), 461–464. \doi{10.1214/aos/1176344136}
#' @references Sinharay, S., & Almond, R. G. (2007). Assessing fit of cognitive
#'   diagnostic models. *Educational and Psychological Measurement, 67*(2),
#'   239-257. \doi{10.1177/0013164406292025}
#' @references Sinharay, S., Johnson, M. S., & Stern, H. S. (2006). Posterior
#'   predictive assessment of item response theory models. *Applied
#'   Psychological Measurement, 30*(4), 298-321.
#'   \doi{10.1177/0146621605285517}
#' @references Thompson, W. J. (2019). *Bayesian psychometrics for diagnostic
#'   assessments: A proof of concept* (Research Report No. 19-01). University
#'   of Kansas; Accessible Teaching, Learning, and Assessment Systems.
#'   \doi{10.35542/osf.io/jzqs8}
#' @references Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian
#'   model evaluation using leave-one-out cross-validation and WAIC.
#'   *Statistics and Computing, 27*(5), 1413-1432.
#'   \doi{10.1007/s11222-016-9696-4}
#' @references Watanabe, S. (2010). Asymptotic equivalence of Bayes cross
#'   validation and widely applicable information criterion in singular learning
#'   theory. *Journal of Machine Learning Research, 11*(116), 3571-3594.
#'   \url{https://jmlr.org/papers/v11/watanabe10a.html}
#'
#' @name model_evaluation
#' @examplesIf measr_examples()
#' cmds_mdm_dina <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "dina",
#'   method = "optim", seed = 63277, backend = "rstan",
#'   prior = c(prior(beta(5, 17), class = "slip"),
#'             prior(beta(5, 17), class = "guess"))
#' )
#'
#' cmds_mdm_dina <- add_reliability(cmds_mdm_dina)
#' cmds_mdm_dina <- add_fit(cmds_mdm_dina, method = "m2")
#' cmds_mdm_dina <- add_respondent_estimates(cmds_mdm_dina)
NULL

#' @export
#' @rdname model_evaluation
add_criterion <- function(x, criterion = c("loo", "waic", "aic", "bic"),
                          overwrite = FALSE, save = TRUE, ..., r_eff = NA) {
  rdcmchecks::check_S7(x, class = "measrfit")
  criterion <- rlang::arg_match(criterion,
                                values = c("loo", "waic", "aic", "bic"),
                                multiple = TRUE)
  check_bool(overwrite)
  check_bool(save)

  # determine which criteria to estimate ---------------------------------------
  new_criteria <- setdiff(criterion, names(x@criteria))
  redo_criteria <- if (overwrite) {
    intersect(criterion, names(x@criteria))
  } else {
    NULL
  }
  all_criteria <- c(new_criteria, redo_criteria)

  if ("loo" %in% all_criteria) {
    x@criteria$loo <- loo(x, r_eff = r_eff, ...)
  }
  if ("waic" %in% all_criteria) {
    x@criteria$waic <- waic(x, ...)
  }
  if ("aic" %in% all_criteria) {
    x@criteria$aic <- aic(x)
  }
  if ("bic" %in% all_criteria) {
    x@criteria$bic <- bic(x)
  }

  # re-save model object (if applicable) ---------------------------------------
  if (!is.null(x@file) && length(all_criteria) && save) {
    write_measrfit(x, file = x@file)
  }
  x
}

#' @export
#' @rdname model_evaluation
add_reliability <- function(x, overwrite = FALSE, save = TRUE, ...) {
  rdcmchecks::check_S7(x, class = "measrfit")
  check_bool(overwrite)
  check_bool(save)

  # determine whether or not to calculate reliability --------------------------
  if (rlang::is_empty(x@reliability) || overwrite) {
    x@reliability <- reliability(x, force = TRUE, ...)
  }

  # re-save model object (if applicable) ---------------------------------------
  if (!is.null(x@file) && save) {
    write_measrfit(x, file = x@file)
  }
  x
}

#' @export
#' @rdname model_evaluation
add_fit <- function(x, method = c("m2", "ppmc"), overwrite = FALSE,
                    save = TRUE, ..., ci = 0.9) {
  rdcmchecks::check_S7(x, class = "measrfit")
  method <- rlang::arg_match(method, values = c("m2", "ppmc"), multiple = TRUE)
  check_bool(overwrite)
  check_bool(save)

  if (S7::S7_inherits(x@method, optim) && "ppmc" %in% method) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(x),
      must = cli::format_message("use {.code method = \"mcmc\"} for PPMC")
    )
  }

  # m2 -------------------------------------------------------------------------
  if ("m2" %in% method &&
      (rlang::is_empty(x@fit$m2) || overwrite)) {
    x@fit$m2 <- fit_m2(x, ci = ci, force = TRUE)
  }

  # ppmc -----------------------------------------------------------------------
  if ("ppmc" %in% method) {
    ppmc_list <- fit_ppmc(x, ..., force = overwrite)
    x@fit <- utils::modifyList(x@fit, ppmc_list)
  }

  # re-save model object (if applicable) ---------------------------------------
  if (!is.null(x@file) && save) {
    write_measrfit(x, file = x@file)
  }
  x
}

#' @export
#' @rdname model_evaluation
add_respondent_estimates <- function(x, probs = c(0.025, 0.975),
                                     overwrite = FALSE, save = TRUE) {
  rdcmchecks::check_S7(x, class = "measrfit")
  check_bool(overwrite)
  check_bool(save)

  for (i in seq_along(probs)) {
    check_number_decimal(probs[i], min = 0, max = 1, arg = "probs")
  }

  if (rlang::is_empty(x@respondent_estimates) || overwrite) {
    x@respondent_estimates <- score(x, summary = TRUE, probs = probs,
                                    force = TRUE)
  }

  # re-save model object (if applicable) ---------------------------------------
  if (!is.null(x@file) && save) {
    write_measrfit(x, file = x@file)
  }
  x
}

#' @export
#' @rdname model_evaluation
add_marginal_likelihood <- function(x, overwrite = FALSE, save = TRUE) {
  if ("<measr::optim>" %in% class(x@method)) {
    rlang::abort("error_bad_method",
                 message = glue::glue("Bayes factor is only ",
                                      "available for models estimated with ",
                                      "`method = \"mcmc\"`."))
  }

  rdcmchecks::check_S7(x, class = "measrfit")
  check_bool(overwrite)
  check_bool(save)

  # calculate log marginal likelihood
  log_marg_lik <- bridgesampling::bridge_sampler(x@model,
                                                 silent = TRUE)

  # save log marginal likelihood to model
  if (rlang::is_empty(x@log_marginal_likelihood) || overwrite) {
    x@log_marginal_likelihood <- list(logml = log_marg_lik$logml)
  }

  # re-save model object (if applicable)
  if (!rlang::is_empty(x@file) && save) {
    write_measrfit(x, file = x@file)
  }

  return(x)
}

#' @export
#' @rdname model_evaluation
add_bayes_factor <- function(x, y, overwrite = FALSE,
                             save = TRUE) {
  if ("<measr::optim>" %in% class(x@method) ||
      "<measr::optim>" %in% class(y@method)) {
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

  x <- add_marginal_likelihood(x = x)
  y <- add_marginal_likelihood(x = y)

  log_marg_lik1 <- x@log_marginal_likelihood$logml
  log_marg_lik2 <- y@log_marginal_likelihood$logml

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
  if (rlang::is_empty(x@bayes_factor) || overwrite) {
    x@bayes_factor <- list(bf = bf,
                           comp_model = y_mod,
                           posterior_probability = posterior_probabilities)
  }

  # re-save model object (if applicable)
  if (!rlang::is_empty(x@file) && save) {
    write_measrfit(x, file = x@file)
  }

  return(x)
}
