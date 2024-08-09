#' Create a `measrfit` object
#'
#' Models fitted with **measr** are represented as a `measrfit` object. If a
#' model is estimated with *Stan*, but not **measr**, a `measrfit` object can be
#' created in order to access other functionality in **measr** (e.g., model fit,
#' reliability).
#'
#' @param data The data and Q-matrix used to estimate the model.
#' @param type The type of DCM that was estimated.
#' @param prior A [measrprior][measrprior()] object containing information on
#'   the priors used in the model.
#' @param stancode The model code in **Stan** language.
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
#' @param version The versions of **measr**, **Stan**, **rstan** and/or
#'   **cmdstanr** that were used to fit the model.
#' @param class Additional classes to be added (e.g., `measrdcm` for a
#'   diagnostic classification model).
#'
#' @return A [measrfit][measrfit-class] object.
#' @export
#' @seealso [measrfit-class], [as_measrfit()], [is_measrfit()]
#' @examplesIf measr_examples()
#' rstn_mdm_lcdm <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' new_obj <- measrfit(
#'   data = rstn_mdm_lcdm$data,
#'   type = rstn_mdm_lcdm$type,
#'   prior = rstn_mdm_lcdm$prior,
#'   stancode = rstn_mdm_lcdm$stancode,
#'   method = rstn_mdm_lcdm$method,
#'   algorithm = rstn_mdm_lcdm$algorithm,
#'   backend = rstn_mdm_lcdm$backend,
#'   model = rstn_mdm_lcdm$model,
#'   respondent_estimates = rstn_mdm_lcdm$respondent_estimates,
#'   fit = rstn_mdm_lcdm$fit,
#'   criteria = rstn_mdm_lcdm$criteria,
#'   reliability = rstn_mdm_lcdm$reliability,
#'   file = rstn_mdm_lcdm$file,
#'   version = rstn_mdm_lcdm$version,
#'   class = "measrdcm"
#' )
measrfit <- function(data = list(), type = character(),
                     prior = default_dcm_priors(type = type),
                     stancode = character(), method = character(),
                     algorithm = character(), backend = character(),
                     model = NULL, respondent_estimates = list(),
                     fit = list(), criteria = list(), reliability = list(),
                     file = NULL, version = list(), class = character()) {
  obj <- list(data = data,
              type = type,
              prior = prior,
              stancode = stancode,
              method = method,
              algorithm = algorithm,
              backend = backend,
              model = model,
              respondent_estimates = respondent_estimates,
              fit = fit,
              criteria = criteria,
              reliability = reliability,
              file = file,
              version = version)

  validate_measrfit(new_measrfit(obj, class = class))
}

#' Coerce objects to a `measrfit`
#'
#' @param x An object to be coerced to a `measrfit`.
#' @param class Additional classes to be added (e.g., `measrdcm` for a
#'   diagnostic classification model).
#'
#' @return An object of class [measrfit-class].
#' @export
#' @seealso [measrfit-class], [measrfit()], [is_measrfit()]
#'
#' @examplesIf measr_examples()
#' rstn_mdm_lcdm <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' new_obj <- as_measrfit(rstn_mdm_lcdm, class = "measrdcm")
as_measrfit <- function(x, class = character()) {
  UseMethod("as_measrfit")
}

#' @export
#' @rdname as_measrfit
as_measrfit.default <- function(x, class = character()) {
  measrfit(
    data = x$data,
    type = x$type,
    prior = x$prior,
    stancode = x$stancode,
    method = x$method,
    algorithm = x$algorithm,
    backend = x$backend,
    model = x$model,
    respondent_estimates = x$respondent_estimates,
    fit = x$fit,
    criteria = x$criteria,
    reliability = x$reliability,
    file = x$file,
    version = x$version,
    class = class
  )
}


#' Class `measrfit` of models fitted with the **measr** package
#'
#' Models fitted with the **measr** package are represented as a `measrfit`
#' object, which contains the posterior draws, Stan code, priors, and other
#' relevant information.
#'
#' @name measrfit-class
#' @docType class
#'
#' @slot data The data and Q-matrix used to estimate the model.
#' @slot type The type of DCM that was estimated.
#' @slot prior A [measrprior][measrprior()] object containing information on the
#'   priors used in the model.
#' @slot stancode The model code in **Stan** language.
#' @slot method The method used to fit the model.
#' @slot algorithm The name of the algorithm used to fit the model.
#' @slot backend The name of the backend used to fit the model.
#' @slot model The fitted Stan model. This will object of class
#'   [rstan::stanfit-class] if `backend = "rstan"` and
#'   [`CmdStanMCMC`](https://mc-stan.org/cmdstanr/reference/CmdStanMCMC.html)
#'   if `backend = "cmdstanr"` was specified when fitting the model.
#' @slot respondent_estimates An empty list for adding estimated person
#'   parameters after fitting the model.
#' @slot fit An empty list for adding model fit information after fitting the
#'   model.
#' @slot criteria An empty list for adding information criteria after fitting
#'   the model.
#' @slot reliability An empty list for adding reliability information after
#'   fitting the model.
#' @slot file Optional name of a file which the model objects was saved to
#'   or loaded from.
#' @slot version The versions of **measr**, **Stan**, **rstan** and/or
#'   **cmdstanr** that were used to fit the model.
#'
#' @seealso [measrfit()], [as_measrfit()], [is_measrfit()]
NULL


new_measrfit <- function(model = list(), ..., class = character()) {
  stopifnot(is.list(model))

  structure(model, ..., class = c(class, "measrfit"))
}

new_measrdcm <- function(x) {
  new_measrfit(x, class = "measrdcm")
}

validate_measrfit <- function(x) {
  # check names ----------------------------------------------------------------
  stopifnot(all(names(x) == c("data", "type", "prior", "stancode", "method",
                              "algorithm", "backend", "model",
                              "respondent_estimates", "fit", "criteria",
                              "reliability", "file", "version")))

  # check types ----------------------------------------------------------------
  stopifnot(is.list(x$data))
  stopifnot(tibble::is_tibble(x$data$data))
  stopifnot(all(colnames(x$data$data) == c("resp_id", "item_id", "score")))
  stopifnot(tibble::is_tibble(x$data$qmatrix))
  stopifnot(is.character(x$data$resp_id))
  stopifnot(is.character(x$data$item_id))
  stopifnot(is.character(x$type))
  stopifnot(x$type %in% dcm_choices())
  stopifnot(is_measrprior(x$prior))
  stopifnot(is.character(x$stancode))
  stopifnot(is.character(x$method))
  stopifnot(x$method %in% c("mcmc", "optim"))
  stopifnot(is.character(x$algorithm))
  stopifnot(is.character(x$backend))
  stopifnot(x$backend %in% backend_choices())
  if (x$backend == "rstan" && x$method == "optim") {
    stopifnot(is.list(x$model))
  } else if (x$backend == "rstan" && x$method == "mcmc") {
    stopifnot(any(class(x$model) == "stanfit"))
  } else if (x$backend == "cmdstanr") {
    stopifnot(any(class(x$model) == "CmdStanFit"))
  }
  stopifnot(is.list(x$respondent_estimates))
  stopifnot(is.list(x$fit))
  stopifnot(is.list(x$criteria))
  stopifnot(is.list(x$reliability))
  stopifnot(is.null(x$file) || is.character(x$file))
  stopifnot(is.list(x$version))
  stopifnot(is.package_version(x$version$R))
  stopifnot(is.package_version(x$version[[2]]))
  stopifnot(is.package_version(x$version$rstan))
  stopifnot(is.package_version(x$version$StanHeaders))
  if (x$backend == "cmdstanr") {
    stopifnot(is.package_version(x$version$cmdstanr))
    stopifnot(is.package_version(x$version$cmdstan))
  }

  x
}


#' Check if argument is a `measrfit` object
#'
#' @param x An object to be checked
#'
#' @return A logical indicating is `x` is a `measrfit` object.
#' @export
#' @seealso [measrfit-class], [measrfit()], [as_measrfit()]
#'
#' @examplesIf measr_examples()
#' rstn_mdm_lcdm <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' is_measrfit(rstn_mdm_lcdm)
is_measrfit <- function(x) {
  inherits(x, "measrfit")
}
