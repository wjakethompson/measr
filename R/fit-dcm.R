#' Fit Bayesian diagnostic classification models
#'
#' Estimate diagnostic classification models (DCMs; also known as cognitive
#' diagnostic models) using 'Stan'. Models can be estimated using Stan's
#' optimizer, or full Markov chain Monte Carlo (MCMC).
#'
#' @param data Response data. A data frame with 1 row per respondent and 1
#'   column per item.
#' @param missing An R expression specifying how missing data in `data` is coded
#'   (e.g., `NA`, `"."`, `-99`, etc.). The default is `NA`.
#' @param qmatrix The Q-matrix. A data frame with 1 row per item and 1 column
#'   per attribute. All cells should be either 0 (item does not measure the
#'   attribute) or 1 (item does measure the attribute).
#' @param resp_id Optional. Variable name of a column in `data` that
#'   contains respondent identifiers. `NULL` (the default) indicates that no
#'   identifiers are present in the data, and row numbers will be used as
#'   identifiers.
#' @param item_id Optional. Variable name of a column in `qmatrix` that contains
#'   item identifiers. `NULL` (the default) indicates that no identifiers are
#'   present in the Q-matrix. In this case, the column names of `data`
#'   (excluding any column specified in `resp_id`) will be used as the item
#'   identifiers. `NULL` also assumes that the order of the rows in the Q-matrix
#'   is the same as the order of the columns in `data` (i.e., the item in row 1
#'   of `qmatrix` is the item in column 1 of `data`, excluding `resp_id`).
#' @param type Type of DCM to estimate. Must be one of
#'   `r glue::glue_collapse(dcm_choices(), sep = ", ", last = ", or ")`.
#' @param method Estimation method. Options are `"mcmc"`, which uses Stan's
#'   sampling method, or `"optim"`, which uses Stan's optimizer.
#' @param prior A [measrprior][measrprior()] object. If `NULL`, default priors
#'   are used, as specified by [default_dcm_priors()].
#' @param backend Character string naming the package to use as the backend for
#'   fitting the Stan model. Options are `"rstan"` (the default) or
#'   `"cmdstanr"`. Can be set globally for the current R session via the
#'   "measr.backend" option (see [options()]). Details on the **rstan** and
#'   **cmdstanr** packages are available at \url{https://mc-stan.org/rstan/} and
#'   \url{https://mc-stan.org/cmdstanr/}, respectively.
#' @param file Either `NULL` (the default) or a character string. If a character
#'   string, the fitted model object is saved as an `.rds` object using
#'   [saveRDS()] using the supplied character string. The `.rds` extension
#'   is automatically added. If the specified file already exists, **measr**
#'   will load the previously saved model. Unless `file_refit` is specified, the
#'   model will not be refit.
#' @param file_refit Controls when a saved model is refit. Options are
#'   `"never"`, `"always"`, and `"on_change"`. Can be set globally for the
#'   current R session via the "measr.file_refit" option (see [options()]).
#'   * For `"never"` (the default), the fitted model is always loaded if the
#'     `file` exists, and model fitting is skipped.
#'   * For `"always"`, the model is always refitted, regardless of whether or
#'     not `file` exists.
#'   * For `"on_change"`, the model will be refit if the `data`, `prior`, or
#'     `method` specified are different from that in the saved `file`.
#' @param ... Additional arguments passed to Stan.
#'   * For `backend = "rstan"`, arguments are passed to [rstan::sampling()]
#'     or [rstan::optimizing()].
#'   * For `backend = "cmdstanr"`, arguments are passed to the
#'     [sample](https://mc-stan.org/cmdstanr/reference/model-method-sample.html)
#'     or
#'     [optimize](https://mc-stan.org/cmdstanr/reference/model-method-optimize.html)
#'     methods of the
#'     [CmdStanModel](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html)
#'     class.
#'
#' @return A [measrfit] object.
#' @export
#' @examplesIf measr_examples()
#' rstn_mdm_lcdm <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
measr_dcm <- function(data,
                      missing = NA,
                      qmatrix,
                      resp_id = NULL,
                      item_id = NULL,
                      type = c("lcdm", "dina", "dino"),
                      method = c("mcmc", "optim"),
                      prior = NULL,
                      backend = getOption("measr.backend", "rstan"),
                      file = NULL,
                      file_refit = getOption("measr.file_refit", "never"),
                      ...) {
  resp_id <- check_character(resp_id, name = "resp_id", allow_null = TRUE)
  item_id <- check_character(item_id, name = "item_id", allow_null = TRUE)
  clean_data <- check_data(data, name = "data", identifier = resp_id,
                           missing = missing)
  qmatrix <- check_qmatrix(qmatrix, identifier = item_id,
                           item_levels = levels(clean_data$item_id),
                           name = "qmatrix")
  clean_qmatrix <- qmatrix %>%
    dplyr::select(-"item_id") %>%
    dplyr::rename_with(~glue::glue("att{1:(ncol(qmatrix) - 1)}"))
  type <- rlang::arg_match(type, dcm_choices())
  method <- rlang::arg_match(method, c("mcmc", "optim"))
  prior <- check_prior(prior, name = "prior", allow_null = TRUE)
  backend <- rlang::arg_match(backend, backend_choices())
  file <- check_file(file, name = "file", create_dir = FALSE,
                     check_file = FALSE, ext = "rds", allow_null = TRUE)
  file_refit <- rlang::arg_match(file_refit, c("never", "always", "on_change"))
  rlang::check_installed(backend,
                         reason = glue::glue("for `backend = \"{backend}\"`"),
                         action = install_backend)

  # create stan infrastructure -----
  stan_data <- create_stan_data(dat = clean_data, qmat = clean_qmatrix,
                                type = type)
  stan_pars <- create_stan_params(backend = backend, method = method, ...)
  stan_pars <- c(list(data = stan_data), stan_pars)

  # compile model -----
  func_name <- rlang::sym(paste0(type, "_script"))
  script_call <- rlang::call2(func_name,
                              rlang::expr(clean_qmatrix),
                              rlang::expr(prior))
  stan_code <- eval(script_call)

  # check for existing file -----
  check <- check_file_exists(file = file, refit = file_refit, dat = clean_data,
                             qmat = qmatrix, code = stan_code, method = method)
  if (check$return) return(check$obj)

  # fit model -----
  if ("precompiled" %in% names(stan_pars)) {
    precompiled <- stan_pars$precompiled
    stan_pars <- stan_pars[which(!names(stan_pars) == "precompiled")]
  } else {
    precompiled <- NULL
  }

  stan_mod <- create_stan_function(backend = backend,
                                   method = method,
                                   code = stan_code,
                                   precompiled = precompiled,
                                   pars = stan_pars,
                                   silent = 2)
  mod <- do.call(stan_mod$func, stan_mod$pars)

  # create measrfit object -----
  algorithm <- extract_algorithm(model = mod, pars = stan_pars, method = method)
  version_info <- get_version_info(cmdstanr = backend == "cmdstanr")

  if (is.null(resp_id)) resp_id <- "resp_id"
  if (is.null(item_id)) item_id <- "item_id"

  ret_mod <- list(data = list(data = clean_data, qmatrix = qmatrix,
                              resp_id = resp_id, item_id = item_id),
                  type = type,
                  prior = stan_code$prior,
                  stancode = stan_code$stancode,
                  method = method,
                  algorithm = algorithm,
                  backend = backend,
                  model = mod,
                  respondent_estimates = list(),
                  fit = list(),
                  criteria = list(),
                  reliability = list(),
                  file = file,
                  version = version_info)
  ret_mod <- new_measrdcm(ret_mod)

  # save and return object -----
  if (!is.null(file)) {
    if (backend == "cmdstanr") {
      ret_mod$model$save_object(gsub("\\.rds", "-cmdstanr.rds", file))
    }
    saveRDS(ret_mod, file = file)
  }
  return(ret_mod)
}
