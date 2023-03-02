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

#' @export
add_criterion <- function(x, criterion, overwrite = FALSE, force_save = FALSE,
                          ...) {

}
