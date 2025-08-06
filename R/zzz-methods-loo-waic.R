#' @importFrom loo loo
#' @export
loo::loo

#' @importFrom loo waic
#' @export
loo::waic

#' @importFrom loo loo_compare
#' @export
loo::loo_compare

#' Relative fit for Bayesian models
#'
#' For models estimated with MCMC, relative model fit comparisons can be made
#' using the LOO-CV or WAIC indicates (Vehtari et al., 2017). These functions
#' are wrappers for the [loo][loo::loo-package] package. See the loo package
#' [vignettes](https://mc-stan.org/loo/articles/) for details on the
#' implementation.
#'
#' @inheritParams loo::loo
#' @param x A [measrdcm][dcm_estimate()] object.
#' @param ... For `loo()` and `waic()`, additional arguments passed to
#'   [loo::loo.array()] or [loo::waic.array()], respectively.
#'   For `loo_compare()`, additional [measrdcm][dcm_estimate()] objects to be
#'   compared to `x`.
#' @param force If the \acronym{LOO} criterion has already been added to the
#'   model object with [add_criterion()], should it be recalculated. Default is
#'   `FALSE`.
#'
#' @concept Bayesian
#'
#' @return For `loo()` and `waic()`, the information criteria returned by
#'   [loo::loo.array()] or [loo::waic.array()], respectively.
#' @name loo-waic
#'
#' @references Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian
#'   model evaluation using leave-one-out cross-validation and WAIC.
#'   *Statistics and Computing, 27*(5), 1413-1432.
#'   \doi{10.1007/s11222-016-9696-4}
#' @export
`loo.measr::measrdcm` <- function(x, ..., r_eff = NA, force = FALSE) {
  if (!rlang::is_empty(x@criteria$loo) && !force) {
    return(x@criteria$loo)
  }

  if (!S7::S7_inherits(x@method, mcmc)) {
    cli::cli_abort(
      glue::glue(
        "{{.arg {rlang::caller_arg(x)}}} must be a model estimated ",
        "with {{.code method = \"mcmc\"}} to estimate the LOO"
      )
    )
  }

  log_lik_array <- loglik_array(x)
  loo::loo(log_lik_array, r_eff = r_eff, ...)
}

#' @rdname loo-waic
#' @export
`waic.measr::measrdcm` <- function(x, ..., force = FALSE) {
  if (!rlang::is_empty(x@criteria$waic) && !force) {
    return(x@criteria$waic)
  }

  if (!S7::S7_inherits(x@method, mcmc)) {
    cli::cli_abort(
      glue::glue(
        "{{.arg {rlang::caller_arg(x)}}} must be a model estimated ",
        "with {{.code method = \"mcmc\"}} to estimate the WAIC"
      )
    )
  }

  log_lik_array <- loglik_array(x)
  loo::waic(log_lik_array, ...)
}

#' @param criterion The name of the criterion to be extracted from the
#'   `x` for comparison.
#' @param model_names Names given to each provided model in the comparison
#'   output. If `NULL` (the default), the names will be parsed from the names of
#'   the objects passed for comparison.
#'
#' @return For `loo_compare()`, the criterion comparison returned by
#'  [loo::loo_compare()].
#' @rdname loo-waic
#' @export
`loo_compare.measr::measrdcm` <- function(
  x,
  ...,
  criterion = c("loo", "waic"),
  model_names = NULL
) {
  dots <- rlang::dots_list(..., .named = TRUE)
  dots_check <- vapply(dots, S7::S7_inherits, logical(1), class = measrdcm)
  if (!all(dots_check)) {
    msg <- paste(
      "{.arg {cli::cli_vec(names(dots)[!dots_check])}} must",
      "{?be a/be a/all be} {.cls measrdcm} object{?s}"
    )
    cli::cli_abort(msg)
  }
  all_models <- c(list(x), dots)

  criterion <- rlang::arg_match(criterion, values = c("loo", "waic"))
  check_character(model_names, allow_null = TRUE)

  if (is.null(model_names)) {
    model_names <- c(rlang::caller_arg(x), names(dots))
  } else if (length(model_names) != length(all_models)) {
    rdcmchecks::abort_bad_argument(
      arg = "model_names",
      must = glue::glue(
        "be of length {length(all_models)}, ",
        "the same as the number of models provided"
      ),
      not = length(model_names)
    )
  }

  all_models <- rlang::set_names(all_models, model_names)
  crit_list <- lapply(all_models, find_criterion, criterion = criterion)
  loo::loo_compare(crit_list)
}

# utilities --------------------------------------------------------------------
find_criterion <- function(model, criterion) {
  if (!rlang::is_empty(model@criteria[[criterion]])) {
    return(model@criteria[[criterion]])
  }

  # fmt: skip
  out <- utils::capture.output( # nolint
    crit <- do.call(criterion, list(x = model))
  )
  crit
}
