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
#' @param x A [measrfit] object.
#' @param criterion A vector of criteria to calculate and add to the model
#'   object.
#' @param method A vector of model fit methods to evaluate and add to the model
#'   object.
#' @param overwrite Logical. Indicates whether existing criteria should be
#'   overwritten. For example, if LOO has already been added to the model object
#'   `add_criterion()` is called again with `criterion = "loo"`, should LOO be
#'   recalculated? Default is `FALSE`.
#' @param save Logical. Only relevant if a file was specified in the
#'   [measrfit] object passed to `x`. If `TRUE` (the default), the model is
#'   re-saved to the specified file when new criteria are added to the R object.
#'   If `FALSE`, the new criteria will be added to the R object, but the saved
#'   file will not be updated.
#' @param ... Additional arguments passed relevant methods. See Details.
#'
#' @details
#' For `add_criterion()`, relative fit criteria are added to the `$criteria`
#' element of the fitted model. This function wraps [loo()] and/or [waic()],
#' depending on which criteria are specified, to calculate the leave-one-out
#' (LOO; Vehtari et al., 2017) and/or widely applicable information criteria
#' (WAIC; Watanabe, 2010) to fitted model objects. Additional arguments supplied
#' to `...` are passed to [loo::loo.array()] or [loo::waic.array()].
#'
#' For `add_reliability()`, reliability information is added to the
#' `$reliability` element of the fitted model. This function wraps
#' [reliability()].
#'
#' For `add_fit()`, model and item fit information are added to the `$model_fit`
#' element of the fitted model. This function wraps [fit_m2()] and/or
#' [fit_ppmc()], depending on which methods are specified. Additional arguments
#' supplied to `...` are passed to [fit_ppmc()].
#'
#' @return A modified [measrfit] object with the `criteria` slot populated with
#'   the specified criteria.
#'
#' @references Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian
#'   model evaluation using leave-one-out cross-validation and WAIC.
#'   *Statistics and Computing, 27*(5), 1413-1432.
#'   \url{https://doi.org/10.1007/s11222-016-9696-4}
#' @references Watanabe, S. (2010). Asymptotic equivalence of Bayes cross
#'   validation and widely applicable information criterion in singular learning
#'   theory. *Journal of Machine Learning Research, 11*(116), 3571-3594.
#'   \url{http://jmlr.org/papers/v11/watanabe10a.html}
#'
#' @name model_evaluation
#' @examplesIf interactive()
#' cmds_mdm_dina <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "dina",
#'   method = "mcmc", seed = 63277, backend = "rstan",
#'   iter = 1500, warmup = 1000, chains = 2,
#'   cores = 2, return_stanfit = FALSE,
#'   prior = c(prior(beta(5, 17), class = "slip"),
#'             prior(beta(5, 17), class = "guess")))
#' )
#'
#' cmds_mdm_dina <- add_criterion(cmds_mdm_dina, criterion = "loo")
#' cmds_mdm_dina <- add_reliability(cmd_mdm_dina)
#' cmds_mdm_dina <- add_fit(cmd_mdm_dina, method = "m2")
NULL

#' @export
#' @rdname model_evaluation
add_criterion <- function(x, criterion = c("loo", "waic"), overwrite = FALSE,
                          save = TRUE, ..., r_eff = NA) {
  model <- check_model(x, required_class = "measrfit", name = "x")
  if (model$method != "mcmc") {
    rlang::abort("error_bad_method",
                 message = glue::glue("Model criteria are only available for ",
                                      "models estimated with ",
                                      "`method = \"mcmc\"`."))
  }
  criterion <- rlang::arg_match(criterion, values = c("loo", "waic"),
                                multiple = TRUE)
  overwrite <- check_logical(overwrite, name = "overwrite")
  save <- check_logical(save, name = "force_save")

  # determine which criteria to estimate
  new_criteria <- setdiff(criterion, names(model$criteria))
  redo_criteria <- if (overwrite) {
    intersect(criterion, names(model$criteria))
  } else {
    NULL
  }
  all_criteria <- c(new_criteria, redo_criteria)

  if (length(all_criteria) > 0) {
    log_lik_array <- prep_loglik_array(model)
  }

  if ("loo" %in% all_criteria) {
    model$criteria$loo <- loo(log_lik_array, r_eff = r_eff)
  }
  if ("waic" %in% all_criteria) {
    model$criteria$waic <- waic(log_lik_array)
  }

  # re-save model object (if applicable)
  if (!is.null(model$file) && length(all_criteria) > 0 && save) {
    saveRDS(model, file = model$file)
  }

  return(model)
}

#' @export
#' @rdname model_evaluation
add_reliability <- function(x, overwrite = FALSE, save = TRUE) {
  model <- check_model(x, required_class = "measrfit", name = "x")
  overwrite <- check_logical(overwrite, name = "overwrite")
  save <- check_logical(save, name = "force_save")

  # determine whether or not to calculate reliability
  run_reli <- length(model$reliability) == 0 || overwrite

  if (run_reli) {
    model$reliability <- reliability(model)
  }

  # re-save model object (if applicable)
  if (!is.null(model$file) && save) {
    saveRDS(model, file = model$file)
  }

  return(model)
}

#' @export
#' @rdname model_evaluation
add_fit <- function(x, method = c("m2", "ppmc"), overwrite = FALSE,
                    save = TRUE, ..., ci = 0.9) {
  model <- check_model(x, required_class = "measrfit", name = "x")
  method <- rlang::arg_match(method, values = c("m2", "ppmc"), multiple = TRUE)
  if ("ppmc" %in% method && model$method != "mcmc") {
    rlang::abort("error_bad_method",
                 message = glue::glue("PPMC is only available for ",
                                      "models estimated with ",
                                      "`method = \"mcmc\"`."))
  }
  overwrite <- check_logical(overwrite, name = "overwrite")
  save <- check_logical(save, name = "force_save")

  # determine whether m2 needs to be run
  run_m2 <- existing_m2_check(model, method, overwrite)

  # determine if/which ppmc need to be run
  dots <- list(...)
  run_ppmc <- existing_ppmc_check(model, method, dots, overwrite)

  if (run_m2) {
    model$model_fit$m2 <- fit_m2(model, ci = ci)
  }
  model <- add_ppmc(model, run_ppmc)

  # re-save model object (if applicable)
  if (!is.null(model$file) && (run_m2 || run_ppmc$run) && save) {
    saveRDS(model, file = model$file)
  }

  return(model)
}
