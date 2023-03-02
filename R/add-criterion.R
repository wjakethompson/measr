#' Add relative model fit criteria to model objects
#'
#' Add leave-one-out (LOO; Vehtari et al., 2017) and/or widely applicable
#' information criteria (WAIC; Watanabe, 2010) to fitted model objects.
#'
#' @inheritParams loo::loo
#' @param x A [measrfit] object.
#' @param criterion A vector of criteria to calculate and add to the model
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
#' @param ... Additional arguments passed to [loo::loo.array()] or
#'   [loo::waic.array()].
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
#' @export
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
  existing_criteria <- names(model$criteria)
  loo_possible <- !("loo" %in% existing_criteria) ||
    (("loo" %in% existing_criteria) & overwrite)
  waic_possible <- !("waic" %in% existing_criteria) ||
    (("waic" %in% existing_criteria) & overwrite)

  run_loo <- loo_possible & ("loo" %in% criterion)
  run_waic <- waic_possible & ("waic" %in% criterion)

  if (run_loo || run_waic) {
    log_lik_array <- prep_loglik_array(model)
  }

  if (run_loo) {
    model$criteria$loo <- loo(log_lik_array, r_eff = r_eff)
  }
  if (run_waic) {
    model$criteria$waic <- waic(log_lik_array)
  }

  # re-save model object (if applicable)
  if (!is.null(model$file) && (run_loo || run_waic) && save) {
    saveRDS(model, file = model$file)
  }

  return(model)
}
