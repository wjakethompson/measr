#' Fit Bayesian diagnostic classification models
#'
#' Something...
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
#' @param return_stanfit Logical. If `backend = "cmdstanr"`, should the fitted
#'   model be coerced to a [rstan::stanfit-class] object in the . Ignored if
#'   `backend = "rstan"`.
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
#'     \code{\link[cmdstanr:model-method-sample]{$sample()}} or
#'     \code{\link[cmdstanr:model-method-optimize]{$optimize()}} methods of the
#'     \link[cmdstanr]{CmdStanModel} class.
#'
#' @return A measrfit object.
#' @export
measr_dcm <- function(data,
                      missing = NA,
                      qmatrix,
                      resp_id = NULL,
                      item_id = NULL,
                      type = c("lcdm", "dina", "dino"),
                      method = c("mcmc", "optim"),
                      prior = NULL,
                      backend = getOption("measr.backend", "rstan"),
                      return_stanfit = TRUE,
                      file = NULL,
                      file_refit = getOption("measr.file_refit", "never"),
                      ...) {
  clean_data <- check_data(data, name = "data", identifier = resp_id,
                           missing = missing)
  qmatrix <- check_qmatrix(qmatrix, identifier = item_id,
                           item_levels = levels(clean_data$item_id),
                           name = "qmatrix")
  clean_qmatrix <- qmatrix %>%
    dplyr::select(-.data$item_id) %>%
    dplyr::rename_with(~glue::glue("att{1:(ncol(qmatrix) - 1)}"))
  resp_id <- check_character(resp_id, name = "resp_id")
  item_id <- check_character(item_id, name = "item_id")
  type <- rlang::arg_match(type, dcm_choices())
  method <- rlang::arg_match(method, c("mcmc", "optim"))
  prior <- check_prior(prior, name = "prior", allow_null = TRUE)
  backend <- rlang::arg_match(backend, backend_choices())
  return_stanfit <- check_logical(return_stanfit, name = "return_stanfit")
  file <- check_file(file, name = "file", create_dir = FALSE,
                     check_file = FALSE, ext = "rds", allow_null = TRUE)
  file_refit <- rlang::arg_match(file_refit, c("never", "always", "on_change"))
  rlang::check_installed(backend,
                         reason = glue::glue("for `backend = \"{backend}\"`"),
                         action = install_backend)

  if (!is.null(file)) {
    if (fs::file_exists(file) && file_refit == "never") {
      return(readRDS(file))
    }
  }

  # create stan data list -----
  ragged_array <- clean_data %>%
    tibble::rowid_to_column() %>%
    dplyr::group_by(.data$resp_id) %>%
    dplyr::summarize(start = min(.data$rowid),
                     num = dplyr::n()) %>%
    dplyr::arrange(.data$resp_id)

  profiles <- create_profiles(ncol(clean_qmatrix))
  xi <- calc_xi(alpha = profiles, qmatrix = clean_qmatrix, type = type)

  stan_data <- list(
    I = length(unique(clean_data$item_id)),
    R = length(unique(clean_data$resp_id)),
    N = nrow(clean_data),
    C = 2 ^ ncol(clean_qmatrix),
    A = ncol(clean_qmatrix),
    ii = as.numeric(clean_data$item_id),
    rr = as.numeric(clean_data$resp_id),
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
  } else if (backend == "rstan" && method == "mcmc") {
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
    defl_pars <- list(algorithm = ifelse(backend == "rstan", "LBFGS", "lbfgs"))
  }
  stan_pars <- utils::modifyList(defl_pars, user_pars)
  stan_pars <- c(list(data = stan_data), stan_pars)

  # compile model -----
  func_name <- rlang::sym(paste0(type, "_script"))
  script_call <- rlang::call2(func_name,
                              rlang::expr(clean_qmatrix),
                              rlang::expr(prior))
  stan_code <- eval(script_call)

  if (!is.null(file)) {
    if (fs::file_exists(file)) {
      prev <- readRDS(file)

      # if fitted model matches current args and "on_change", return prev fit
      if (all(identical(prev$data, list(data = clean_data, qmatrix = qmatrix)),
              identical(prev$prior, stan_code$prior),
              identical(prev$method, method)) &&
          file_refit == "on_change") {
        return(prev)
      }
    }
  }

  if (backend == "rstan") {
    comp_mod <- rstan::stan_model(model_code = stan_code$stancode)
    stan_pars$object <- comp_mod
    fit_func <- switch(method,
                       mcmc = rstan::sampling,
                       optim = rstan::optimizing)
  } else if (backend == "cmdstanr") {
    comp_mod <- cmdstanr::cmdstan_model(
      cmdstanr::write_stan_file(stan_code$stancode),
      compile = FALSE
    )
    comp_mod$format(overwrite_file = TRUE, canonicalize = TRUE, quiet = TRUE)
    comp_mod <- comp_mod$compile()
    fit_func <- switch(method,
                       mcmc = comp_mod$sample,
                       optim = comp_mod$optimize)
  }

  # fit model -----
  mod <- do.call(fit_func, stan_pars)
  if (backend == "cmdstanr" && method == "mcmc" && return_stanfit) {
    mod <- rstan::read_stan_csv(mod$output_files())
    mod <- fix_cmdstanr_names(mod)
  }

  # create measrfit object -----
  algorithm <- if (method == "optim") {
    stan_pars$algorithm
  } else if ("stanfit" %in% class(mod)) {
    mod@stan_args[[1]]$algorithm
  } else if ("CmdStanFit" %in% class(mod)) {
    mod$metadata()$algorithm
  }
  version_info <- list(measr = utils::packageVersion("measr"),
                       rstan = utils::packageVersion("rstan"),
                       StanHeaders = utils::packageVersion("StanHeaders"))
  if (backend == "cmdstanr") {
    version_info$cmdstanr <- utils::packageVersion("cmdstanr")
    version_info$cmdstan <- as.package_version(cmdstanr::cmdstan_version())
  }

  ret_mod <- list(data = list(data = clean_data, qmatrix = qmatrix),
                  prior = stan_code$prior,
                  stancode = stan_code$stancode,
                  method = method,
                  algorithm = algorithm,
                  backend = backend,
                  model = mod,
                  model_fit = list(),
                  criteria = list(),
                  reliability = list(),
                  file = file,
                  version = version_info)
  ret_mod <- new_measrdcm(ret_mod)

  # save and return object -----
  if (!is.null(file)) {
    saveRDS(ret_mod, file = file)
  }
  return(ret_mod)
}
