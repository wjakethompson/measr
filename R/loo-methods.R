#' @importFrom loo loo
#' @export
loo::loo

#' @importFrom loo waic
#' @export
loo::waic

#' @importFrom loo loo_compare
#' @export
loo::loo_compare

#' Efficient approximate leave-one-out cross-validation (LOO)
#'
#' A [loo::loo()] method that is customized for `measrfit` objects. This is a
#' simple wrapper around [loo::loo.array()]. See the **loo** package
#' [vignettes](https://mc-stan.org/loo/articles/) for details.
#'
#' @inheritParams loo::loo
#' @param x A [measrfit] object.
#' @param ... Additional arguments passed to [loo::loo.array()].
#'
#' @return The object returned by [loo::loo.array()].
#'
#' @export
loo.measrfit <- function(x, ..., r_eff = NA) { #nolint
  model <- check_model(x, required_class = "measrfit", name = "x")

  if (model$method != "mcmc") {
    rlang::abort("error_bad_method",
                 message = glue::glue("LOO-CV is only available for models ",
                                      "estimated with `method = \"mcmc\"`."))
  }

  log_lik_array <- prep_loglik_array(model)

  loo::loo(log_lik_array, r_eff = r_eff, ...)
}

#' Widely applicable information criterion (WAIC)
#'
#' A [loo::waic()] method that is customized for `measrfit` objects. This is a
#' simple wrapper around [loo::waic.array()]. See the **loo** package
#' [vignettes](https://mc-stan.org/loo/articles/) for details.
#'
#' @param x A [measrfit] object.
#' @param ... Additional arguments passed to [loo::waic.array()].
#'
#' @return The object returned by [loo::waic.array()].
#'
#' @export
waic.measrfit <- function(x, ...) { #nolint
  model <- check_model(x, required_class = "measrfit", name = "x")

  if (model$method != "mcmc") {
    rlang::abort("error_bad_method",
                 message = glue::glue("WAIC is only available for models ",
                                      "estimated with `method = \"mcmc\"`."))
  }

  log_lik_array <- prep_loglik_array(model)

  loo::waic(log_lik_array, ...)
}

#' Relative model fit comparisons
#'
#' A [loo::loo_compare()] method that is customized for `measrfit` objects. See
#' the **loo** package [vignettes](https://mc-stan.org/loo/articles/) for
#' details.
#'
#' @param x A [measrfit] object.
#' @param ... Additional objects of class [measrfit].
#' @param criterion The name of the criterion to be extracted from the
#'   [measrfit] object for comparison.
#' @param model_names Names given to each provided model in the comparison
#'   output. If `NULL` (the default), the names will be parsed from the names of
#'   the objects passed for comparison.
#'
#' @return The object returned by [loo::loo_compare()].
#'
#' @export
loo_compare.measrfit <- function(x, ..., criterion = c("loo", "waic"),
                                 model_names = NULL) {
  obj_nms <- list_names(x, ...)

  x <- check_model(x, required_class = "measrfit", name = "x")
  ext_models <- list(...)
  ext_models <- lapply(ext_models, check_model, required_class = "measrfit",
                       name = "...", list = TRUE)
  all_models <- c(list(x), ext_models)

  criterion <- rlang::arg_match(criterion, values = c("loo", "waic"))
  model_names <- vapply(model_names, check_character, character(1),
                        allow_null = TRUE, name = "model_names",
                        USE.NAMES = FALSE)

  if (length(model_names) == 0) {
    model_names <- obj_nms
  } else if (length(model_names) != length(all_models)) {
    abort_bad_argument(arg = "model_names",
                       must = glue::glue("be of length {length(all_models)}, ",
                                         "the same as the number of models ",
                                         "provided"),
                       not = length(model_names))
  }

  loo_list <- lapply(all_models, \(x) x[["criteria"]][[criterion]])
  loo_list <- rlang::set_names(loo_list, nm = model_names)
  missing_crit <- vapply(loo_list, is.null, logical(1))

  if (any(missing_crit)) {
    rlang::abort("error_missing_criterion",
                 message = glue::glue("Model {which(missing_crit)} does not ",
                                      "contain a precomputed \"{criterion}\". ",
                                      "See `?measr::add_criterion()` for ",
                                      "help."))
  }

  loo::loo_compare(loo_list)
}
