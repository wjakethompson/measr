#' Fit Bayesian diagnostic classification models
#'
#' Something...
#'
#' @param data Response data. A data frame with 1 row per respondent and 1
#'   column per item.
#' @param resp_id Optional. Variable name of a column in `data` that
#'   contains respondent identifiers. `NULL` (the default) indicates that no
#'   identifiers are present in the data, and row numbers will be used as
#'   identifiers.
#' @param missing An R expression specifying how missing data is coded (e.g.,
#'   `NA`, `"."`, `-99`, etc.). The default is `NA`.
#' @param qmatrix The Q-matrix. A data frame with 1 row per item and 1 column
#'   per attribute. All cells should be either 0 (item does not measure the
#'   attribute) or 1 (item does measure the attribute).
#' @param type Type of DCM to estimate. Currently only the LCDM is supported.
#' @param method Estimation method. Options are `"mcmc"`, which uses Stan's
#'   sampling method, or `"optim"`, which uses Stan's optimizer.
#' @param prior A [measrprior][measrprior()] object. If `NULL`, default priors
#'   are used, as specified by [default_dcm_priors()].
#' @param backend Character string naming the package to use as the backend for
#'   fitting the Stan model. Options are "rstan" (the default) or "cmdstanr".
#'   Can be set globally for the current R session via the "measr.backend"
#'   option (see options). Details on the rstan and cmdstanr packages are
#'   available at \url{https://mc-stan.org/rstan/} and
#'   \url{https://mc-stan.org/cmdstanr/}, respectively.
#' @param ... Additional arguments passed to Stan.
#'   * For `backend = "rstan"`, arguments are passed to [rstan::sampling()]
#'     or [rstan::optimizing()].
#'   * For `backend = "cmdstanr"`, arguments are passed to the
#'     \code{\link[cmdstanr:model-method-sample]{$sample()}} or
#'     \code{\link[cmdstanr:model-method-optimize]{$optimize()}} methods of the
#'     \link[cmdstanr]{CmdStanModel} class.
#'
#' @return A measrfit object.
#' @export
measr_dcm <- function(data, resp_id = NULL, missing = NA, qmatrix,
                      type = c("lcdm"), method = c("mcmc", "optim"),
                      prior = NULL,
                      backend = getOption("measr.backend", "rstan"), ...) {
  clean_data <- check_data(data, name = "data", identifier = resp_id,
                           missing = missing)
  qmatrix <- check_qmatrix(qmatrix, name = "qmatrix")
  clean_qmatrix <- dplyr::rename_with(qmatrix,
                                      ~glue::glue("att{1:ncol(qmatrix)}"))
  type <- rlang::arg_match(type, dcm_choices())
  method <- rlang::arg_match(method)
  prior <- check_prior(prior, name = "prior", allow_null = TRUE)
  backend <- rlang::arg_match(backend, backend_choices())
  rlang::check_installed(backend,
                         reason = glue::glue("for `backend = \"{backend}\"`"),
                         action = install_backend)


}
