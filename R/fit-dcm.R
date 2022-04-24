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
#' @param type Type of DCM to estimate. Must be one of
#'   `r glue::glue_collapse(dcm_choices(), sep = ", ", last = ", or ")`.
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
                      type = c("lcdm", "dina", "dino"),
                      method = c("mcmc", "optim"),
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

  # create stan data list -----
  ragged_array <- clean_data %>%
    tibble::rowid_to_column() %>%
    dplyr::group_by(stan_resp_id) %>%
    dplyr::summarize(start = min(.data$rowid),
                     num = dplyr::n()) %>%
    dplyr::arrange(.data$stan_resp_id)

  profiles <- create_profiles(ncol(clean_qmatrix))
  xi <- calc_xi(alpha = profiles, qmatrix = clean_qmatrix, type = type)

  stan_data <- list(
    I = length(unique(clean_data$stan_item_id)),
    R = length(unique(clean_data$stan_resp_id)),
    N = nrow(clean_data),
    C = 2 ^ ncol(clean_qmatrix),
    A = ncol(clean_qmatrix),
    ii = clean_data$stan_item_id,
    rr = clean_data$stan_resp_id,
    y = clean_data$score,
    start = ragged_array$start,
    num = ragged_array$num,
    Alpha = profiles,
    Xi = xi
  )

  # stan parameters -----
  ## user defined
  user_pars <- list(...)
  user_names <- names(user_pars)
  if ("control" %in% user_names) {
    new_control <- utils::modifyList(list(adapt_delta = 0.95),
                                     user_pars$control)
    user_pars$control <- new_control
  } else if (backend == "rstan" & method == "mcmc") {
    user_pars$control <- list(adapt_delta = 0.95)
  }

  ## some reasonable defaults
  if (method == "mcmc") {
    if (backend == "rstan") {
      defl_pars <- list(iter = 4000, warmup = 2000, chains = 4,
                        cores = getOption("mc.cores", 1L))
    } else if (backend == "cmdstanr") {
      defl_pars <- list(iter_sampling = 2000, iter_warmup = 2000, chains = 4,
                        parallel_chains = getOption("mc.cores", 1L),
                        adapt_delta = 0.95)
    }
  } else if (method == "optim") {
    defl_pars <- list()
  }
  stan_pars <- utils::modifyList(defl_pars, user_pars)
  stan_pars <- c(list(data = stan_data), stan_pars)

  # compile model -----
  func_name <- rlang::sym(paste0(type, "_script"))
  script_call <- rlang::call2(func_name,
                              rlang::expr(qmatrix),
                              rlang::expr(prior))
  stan_code <- eval(script_call)

  if (backend == "rstan") {
    comp_mod <- rstan::stan_model(model_code = stan_code)
    stan_pars$object <- comp_mod
    fit_func <- switch(method,
                       mcmc = rstan::sampling,
                       optim = rstan::optimizing)
  } else if (backend == "cmdstanr") {
    comp_mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(stan_code))
    fit_func <- switch(method,
                       mcmc = comp_mod$sample,
                       optim = comp_mod$optimize)
  }

  # fit model -----
  mod <- do.call(fit_func, stan_pars)
}
