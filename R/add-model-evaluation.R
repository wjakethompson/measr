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
  if (!rlang::is_empty(x@file) && length(all_criteria) && save) {
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
  if (!rlang::is_empty(x@file) && save) {
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
  if (!rlang::is_empty(x@file) && save) {
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
  if (!rlang::is_empty(x@file) && save) {
    write_measrfit(x, file = x@file)
  }
  x
}

#' @export
#' @rdname model_evaluation
add_yens_q <- function(x, crit_value = .2,
                       overwrite = FALSE, save = TRUE) {
  if (rlang::is_empty(x@respondent_estimates)) {
    rlang::abort("error_bad_method",
                 message = glue::glue("Run `add_respondent_estimates()` ",
                                      "before `add_yens_q()`."))
  }

  possible_profs <- create_profiles(x@data@qmatrix |>
                                      dplyr::select(
                                        -!!rlang::sym(x@data@item_id)
                                      ) |>
                                      ncol()) |>
    tidyr::unite(col = "profile", everything(), sep = "") |>
    tibble::rowid_to_column("profile_num")

  qmatrix <- x@data@qmatrix |>
    dplyr::select(-!!rlang::sym(x@data@item_id))

  obs <- x@data@data |>
    dplyr::rename(resp_id = !!rlang::sym(x@data@resp_id),
                  item_id = !!rlang::sym(x@data@item_id))
  # pi matrix
  item_param <- measr_extract(x, "item_param")
  item_param <- item_param |>
    dplyr::mutate(estimate = mean(.data$estimate)) |>
    dplyr::select(!!dplyr::sym(x@data$item_identifier):"estimate")
  item_ids <- item_param |>
    dplyr::distinct(.data$item_id) %>%
    tibble::rowid_to_column("new_item_id")
  item_param <- item_param |>
    dplyr::left_join(item_ids, by = "item_id") |>
    dplyr::mutate(item_id = .data$new_item_id) |>
    dplyr::select(-"new_item_id")
  all_params <- item_param |>
    dplyr::select(-"estimate") |>
    dplyr::rename(type = "class", coefficient = "coef")

  att_dict <- tibble::tibble(att = colnames(qmatrix)) |>
    tibble::rowid_to_column("att_name") |>
    dplyr::mutate(att_name = glue::glue("att{att_name}"))

  for (ii in att_dict$att) {
    all_params <- all_params |>
      dplyr::mutate(attributes = sub(ii,
                                     att_dict |>
                                       dplyr::filter(.data$att == ii) |>
                                       dplyr::pull(.data$att_name),
                                     .data$attributes))
  }

  all_params <- all_params |>
    dplyr::mutate(attributes = dplyr::case_when(is.na(.data$attributes) ~
                                                  "intercept",
                                                TRUE ~ .data$attributes))

  all_profiles <- create_profiles(ncol(qmatrix))

  profile_params <-
    stats::model.matrix(stats::as.formula(paste0("~ .^",
                                                 max(ncol(all_profiles),
                                                     2L))),
                        all_profiles) |>
    tibble::as_tibble() |>
    tibble::rowid_to_column(var = "profile_id") |>
    tidyr::pivot_longer(-"profile_id", names_to = "parameter",
                        values_to = "valid_for_profile") |>
    dplyr::mutate(parameter = dplyr::case_when(.data$parameter ==
                                                 "(Intercept)" ~
                                                 "intercept",
                                               TRUE ~ gsub(":", "__",
                                                           .data$parameter)))

  pi_mat <- tidyr::expand_grid(item_id = seq_len(nrow(qmatrix)),
                               profile_id = seq_len(nrow(all_profiles))) |>
    dplyr::left_join(dplyr::select(all_params, "item_id",
                                   "parameter" = "attributes",
                                   "param_name" = "coefficient"),
                     by = "item_id",
                     multiple = "all", relationship = "many-to-many") |>
    dplyr::left_join(profile_params, by = c("profile_id", "parameter"),
                     relationship = "many-to-one") |>
    dplyr::filter(.data$valid_for_profile == 1) |>
    dplyr::select(-"valid_for_profile") |>
    dplyr::left_join(item_param |>
                       dplyr::select("coef", "estimate"),
                     by = c("param_name" = "coef")) |>
    dplyr::group_by(.data$item_id, .data$profile_id) |>
    dplyr::summarize(log_odds = sum(.data$estimate),
                     .groups = "drop") |>
    dplyr::mutate(prob = 1 / (1 + exp(-.data$log_odds))) |>
    dplyr::select(-"log_odds") |>
    dplyr::rename(pi = "prob")
  class_probs <- x@respondent_estimates@class_probabilities |>
    dplyr::mutate(class = gsub("(\\D|\\.(?=.*\\.))", "", .data$class,
                               perl = TRUE)) |>
    dplyr::left_join(possible_profs, by = c("class" = "profile")) |>
    dplyr::select(resp_id = !!rlang::sym(x@data@resp_id), class = "profile_num",
                  "probability")

  exp_value <- class_probs |>
    dplyr::left_join(pi_mat, by = c("class" = "profile_id"),
                     relationship = "many-to-many") |>
    dplyr::mutate(exp = .data$probability * .data$pi) |>
    dplyr::group_by(.data$resp_id, .data$item_id) |>
    dplyr::summarize(exp = sum(.data$exp), .groups = 'keep') |>
    dplyr::ungroup() |>
    dplyr::left_join(obs |>
                       dplyr::left_join(item_ids, by = c("item_id")) |>
                       dplyr::select("resp_id", item_id = "new_item_id",
                                     "score"),
                     by = c("resp_id", "item_id")) |>
    dplyr::mutate(d = .data$score - .data$exp)

  # calculate correlation matrix for all item combos
  num_items <- nrow(item_ids)
  yens_q <- matrix(nrow = num_items, ncol = num_items)

  for (ii in seq_len(num_items)) {
    for (jj in seq_len(num_items)) {
      if (ii == jj) {
        yens_q[ii, jj] <- 1
      } else {
        tmp_yens_q <- exp_value |>
          dplyr::filter(item_id == ii | item_id == jj) |>
          dplyr::select(-"exp", -"score") |>
          tidyr::pivot_wider(names_from = "item_id", values_from = "d") |>
          dplyr::select(-"resp_id") |>
          cor()

        yens_q[ii, jj] <- tmp_yens_q[1,2]
        yens_q[jj, ii] <- tmp_yens_q[2,1]
      }
    }
  }

  yens_q <- yens_q |>
    tibble::as_tibble()
  names(yens_q) <- item_ids |>
    dplyr::mutate(item_id = as.character(.data$new_item_id)) |>
    dplyr::pull()
  yens_q <- yens_q |>
    dplyr::mutate(item_id = as.character(item_ids$new_item_id)) |>
    tidyr::pivot_longer(cols = -c("item_id"), names_to = "item_id_2",
                        values_to = "resid_corr") |>
    dplyr::filter(.data$item_id < .data$item_id_2) |>
    dplyr::mutate(flag = abs(resid_corr) >= crit_value)

  if (overwrite || rlang::is_empty(x@yens_q)) {
    x@yens_q <- list(yens_q = yens_q)
  }

  # re-save model object (if applicable)
  if (!is.null(x@file) && save) {
    write_measrfit(x, file = x@file)
  }

  return(x)
}
