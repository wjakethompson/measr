#' Fit Bayesian diagnostic classification models
#'
#' Estimate diagnostic classification models (\acronym{DCM}s; also known as
#' cognitive diagnostic models) using 'Stan'. Models can be estimated using
#' Stan's optimizer, or full Markov chain Monte Carlo (\acronym{MCMC}).
#'
#' @param dcm_spec A DCM specification created with [dcm_specify()].
#' @param data Response data. A data frame with 1 row per respondent and 1
#'   column per item.
#' @param missing An `R` expression specifying how missing data in `data` is
#'   coded (e.g., `NA`, `"."`, `-99`, etc.). The default is `NA`.
#' @param identifier Optional. Variable name of a column in `data` that
#'   contains respondent identifiers. `NULL` (the default) indicates that no
#'   identifiers are present in the data, and row numbers will be used as
#'   identifiers.
#' @param method Estimation method. Options are `"mcmc"`, which uses Stan's
#'   sampling method, or `"optim"`, which uses Stan's optimizer.
#' @param backend Character string naming the package to use as the backend for
#'   fitting the Stan model. Options are `"rstan"` (the default) or
#'   `"cmdstanr"`. Can be set globally for the current `R` session via the
#'   "measr.backend" option (see [options()]). Details on the rstan and
#'   cmdstanr packages are available at \url{https://mc-stan.org/rstan/} and
#'   \url{https://mc-stan.org/cmdstanr/}, respectively.
#' @param file Either `NULL` (the default) or a character string. If a character
#'   string, the fitted model object is saved as an `.rds` object using
#'   [saveRDS()] using the supplied character string. The `.rds` extension
#'   is automatically added. If the specified file already exists, measr
#'   will load the previously saved model. Unless `file_refit` is specified, the
#'   model will not be refit.
#' @param file_refit Controls when a saved model is refit. Options are
#'   `"never"`, `"always"`, and `"on_change"`. Can be set globally for the
#'   current `R` session via the "measr.file_refit" option (see [options()]).
#'   * For `"never"` (the default), the fitted model is always loaded if the
#'     `file` exists, and model fitting is skipped.
#'   * For `"always"`, the model is always refitted, regardless of whether or
#'     not `file` exists.
#'   * For `"on_change"`, the model will be refit if the `dcm_spec`, `data`,
#'     `method`, or `backend` specified are different from that in the saved
#'     `file`.
#' @param ... Additional arguments passed to Stan.
#'   * For `backend = "rstan"`, arguments are passed to [rstan::sampling()]
#'     or [rstan::optimizing()].
#'   * For `backend = "cmdstanr"`, arguments are passed to the
#'     [`$sample()`](https://mc-stan.org/cmdstanr/reference/model-method-sample.html)
#'     or
#'     [`$optimize()`](https://mc-stan.org/cmdstanr/reference/model-method-optimize.html)
#'     methods of the
#'     [CmdStanModel](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html)
#'     class.
#'
#' @concept Bayesian
#' @concept DCM
#' @concept Stan
#'
#' @returns A `measrdcm` object.
#' @export
#'
#' @examplesIf measr_examples()
#' model_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
#'                           identifier = "item")
#' model <- dcm_estimate(dcm_spec = model_spec, data = dcmdata::mdm_data,
#'                       identifier = "respondent", method = "optim",
#'                       seed = 63277)
dcm_estimate <- function(dcm_spec, data, missing = NA, identifier = NULL,
                         method = c("mcmc", "optim"),
                         backend = getOption("measr.backend", "rstan"),
                         file = NULL,
                         file_refit = getOption("measr.file_refit", "never"),
                         ...) {
  # check function inputs ------------------------------------------------------
  rdcmchecks::check_S7(dcm_spec, "dcmstan::dcm_specification")
  check_string(identifier, allow_null = TRUE)
  clean_data <- rdcmchecks::clean_data(
    data, identifier = identifier, missing = missing,
    cleaned_qmatrix = list(
      clean_qmatrix = dcm_spec@qmatrix,
      attribute_names = dcm_spec@qmatrix_meta$attribute_names,
      item_identifier = dcm_spec@qmatrix_meta$item_identifier,
      item_names = dcm_spec@qmatrix_meta$item_names
    ),
    arg_qmatrix = "dcm_spec"
  )
  method <- rlang::arg_match(method, values = c("mcmc", "optim"))
  backend <- rlang::arg_match(backend, values = c("rstan", "cmdstanr"))
  file <- check_file(file, create_dir = FALSE, check_file = FALSE, ext = "rds",
                     allow_null = TRUE)
  file_refit <- rlang::arg_match(file_refit,
                                 values = c("never", "always", "on_change"))

  # initial return check -------------------------------------------------------
  if (length(file) && file.exists(file) && file_refit == "never") {
    return(read_measrfit(file))
  }

  # infrastructure classes -----------------------------------------------------
  stan_mthd <- do.call(method, args = list())
  stan_bknd <- do.call(backend, args = list())

  # stan infrastructure --------------------------------------------------------
  stan_code <- dcmstan::stan_code(x = dcm_spec)
  stan_dat <- dcmstan::stan_data(x = dcm_spec, data = data,
                                 missing = missing, identifier = identifier)
  stan_args <- utils::modifyList(
    default_stan_args(backend = stan_bknd, method = stan_mthd,
                      user_args = list(...)),
    list(...)
  )
  stan_args <- c(list(data = stan_dat), stan_args)

  # check for changed file -----------------------------------------------------
  if (length(file) && file.exists(file) && file_refit == "on_change") {
    previous_fit <- check_previous_fit(
      file = file, dcm_spec = dcm_spec, clean_data = clean_data,
      stan_mthd = stan_mthd, stan_bknd = stan_bknd
    )
    if (!is.null(previous_fit)) return(previous_fit)
  }

  # fit model ------------------------------------------------------------------
  if ("precompiled" %in% names(stan_args)) {
    precompiled <- stan_args$precompiled
    stan_args <- stan_args[which(!names(stan_args) == "precompiled")]
  } else {
    precompiled <- NULL
  }

  stan_function_call <- stan_call(backend = stan_bknd, method = stan_mthd,
                                  code = stan_code, args = stan_args,
                                  precompiled = precompiled)
  mod <- do.call(stan_function_call$call_function,
                 stan_function_call$args)

  # create measrdcm object -----------------------------------------------------
  new_spec <- dcmstan::dcm_specification(
    qmatrix = dcm_spec@qmatrix,
    qmatrix_meta = list(attribute_names = dcm_spec@qmatrix_meta$attribute_names,
                        item_identifier = clean_data$item_identifier,
                        item_names = clean_data$item_names),
    measurement_model = dcm_spec@measurement_model,
    structural_model = dcm_spec@structural_model,
    priors = dcm_spec@priors
  )

  mod_obj <- measrdcm(
    model_spec = new_spec,
    data = clean_data,
    stancode = stan_code,
    method = stan_mthd,
    algorithm = get_algorithm(stan_bknd, stan_mthd, args = stan_args,
                              model = mod),
    backend = stan_bknd,
    model = mod,
    file = file,
    version = get_version_info(stan_bknd)
  )

  if (length(file)) {
    write_measrfit(model = mod_obj, file)
  }

  mod_obj
}


# measrfit class ---------------------------------------------------------------
#' S7 class for measrfit objects
#'
#' @param model_spec The model specification used to estimate the model.
#' @param data The data used to estimate the model.
#' @param stancode The model code in *Stan* language.
#' @param method The method used to fit the model.
#' @param algorithm The name of the algorithm used to fit the model.
#' @param backend The name of the backend used to fit the model.
#' @param model The fitted Stan model. This will object of class
#'   [rstan::stanfit-class] if `backend = "rstan"` and
#'   [`CmdStanMCMC`](https://mc-stan.org/cmdstanr/reference/CmdStanMCMC.html)
#'   if `backend = "cmdstanr"` was specified when fitting the model.
#' @param respondent_estimates An empty list for adding estimated person
#'   parameters after fitting the model.
#' @param fit An empty list for adding model fit information after fitting the
#'   model.
#' @param criteria An empty list for adding information criteria after fitting
#'   the model.
#' @param reliability An empty list for adding reliability information after
#'   fitting the model.
#' @param file Optional name of a file which the model objects was saved to
#'   or loaded from.
#' @param version The versions of measr, *Stan*, and rstan or cmdstanr that were
#'   used to fit the model.
#'
#' @concept Stan
#'
#' @noRd
measrfit <- S7::new_class("measrfit", package = "measr",
  properties = list(
    model_spec = S7::new_property(
      class = S7::S7_object,
      setter = function(self, value) {
        if (!is.null(self@model_spec)) {
          stop("@model_spec is read-only", call. = FALSE)
        }
        self@model_spec <- value
        self
      }
    ),
    data = S7::new_property(
      class = S7::class_list,
      setter = function(self, value) {
        if (!is.null(self@data)) {
          stop("@data is read-only", call. = FALSE)
        }
        self@data <- value
        self
      }
    ),
    stancode = S7::new_property(
      class = S7::new_S3_class(class = c("glue", "character")),
      default = glue::glue(),
      setter = function(self, value) {
        if (!is.null(self@stancode)) {
          stop("@stancode is read-only", call. = FALSE)
        }
        self@stancode <- value
        self
      }
    ),
    method = S7::new_property(
      class = stanmethod,
      default = optim(),
      setter = function(self, value) {
        if (!is.null(self@method)) {
          stop("@method is read-only", call. = FALSE)
        }
        self@method <- value
        self
      }
    ),
    algorithm = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        if (!is.null(self@algorithm)) {
          stop("@algorithm is read-only", call. = FALSE)
        }
        self@algorithm <- value
        self
      }
    ),
    backend = S7::new_property(
      class = stanbackend,
      default = rstan(),
      setter = function(self, value) {
        if (!is.null(self@backend)) {
          stop("@backend is read-only", call. = FALSE)
        }
        self@backend <- value
        self
      }
    ),
    model = S7::new_property(
      class = S7::class_any,
      default = list(),
      setter = function(self, value) {
        if (!is.null(self@model)) {
          stop("@model is read-only", call. = FALSE)
        }
        self@model <- value
        self
      }
    ),
    respondent_estimates = S7::class_list,
    fit = S7::class_list,
    criteria = S7::class_list,
    reliability = S7::class_list,
    file = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        if (!is.null(self@file)) {
          stop("@file is read-only", call. = FALSE)
        }
        self@file <- value
        self
      }
    ),
    version = S7::new_property(
      class = S7::class_list,
      setter = function(self, value) {
        if (!is.null(self@version)) {
          stop("@version is read-only", call. = FALSE)
        }
        self@version <- value
        self
      }
    )
  ),
  validator = function(self) {
    err <- if ((inherits(self@backend, "measr::rstan") &&
                inherits(self@method, "measr::optim")) &&
               !is.list(self@model)) {
      cli::cli_fmt(cli::cli_text("@model must be a list returned by ",
                                 "{.fun rstan::optimizing}"))
    } else if ((inherits(self@backend, "measr::rstan") &&
                inherits(self@method, "measr::mcmc")) &&
               !inherits(self@model, "stanfit")) {
      cli::cli_fmt(cli::cli_text("@model must be a {.cls stanfit} object ",
                                 "returned by {.fun rstan::sampling}"))
    } else if ((inherits(self@backend, "measr::cmdstanr") &&
                inherits(self@method, "measr::optim")) &&
               !inherits(self@model, "CmdStanMLE")) {
      cli::cli_fmt(cli::cli_text(
        "@model must be a {.cls CmdStanMLE} object returned by the ",
        "{.help [{.fun $optimize}](cmdstanr::CmdStanMLE)} method"
      ))
    } else if ((inherits(self@backend, "measr::cmdstanr") &&
                inherits(self@method, "measr::mcmc")) &&
               !inherits(self@model, "CmdStanMCMC")) {
      cli::cli_fmt(cli::cli_text(
        "@model must be a {.cls CmdStanMCMC} object returned by the ",
        "{.help [{.fun $sample}](cmdstanr::CmdStanMCMC)} method"
      ))
    } else {
      NULL
    }

    if (!is.null(err)) err
  }
)

measrdcm <- S7::new_class("measrdcm", parent = measrfit, package = "measr",
  properties = list(
    model_spec = S7::new_property(class = dcmstan::dcm_specification)
  )
)
