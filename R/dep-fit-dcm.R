#' Fit Bayesian diagnostic classification models
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `measr_dcm()` has been deprecated in favor of [dcm_estimate()]. Please use
#' [dcm_estimate()], as `measr_dcm()` will be removed in a future release.
#'
#' @param data Response data. A data frame with 1 row per respondent and 1
#'   column per item.
#' @param missing An `R` expression specifying how missing data in `data` is
#'   coded (e.g., `NA`, `"."`, `-99`, etc.). The default is `NA`.
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
#' @param type Type of \acronym{DCM} to estimate. Must be one of
#'   `"lcdm"`, `"dina"`, `"dino"`, or `"crum"`.
#' @param max_interaction If `type = "lcdm"`, the highest level of interaction
#'   to estimate. The default is to estimate all possible interactions. For
#'   example, an item that measures 4 attributes would have 4 main effects,
#'   6 two-way interactions, 4 three-way interactions, and 1 four-way
#'   interaction. Setting `max_interaction = 2` would result in only estimating
#'   the main effects and two-way interactions, excluding the three- and four-
#'   way interactions.
#' @param attribute_structure Structural model specification. Must be one of
#'   `"unconstrained"` or `"independent"`.
#'   `"unconstrained"` makes no assumptions about the relationships between
#'   attributes, whereas `"independent"` assumes that proficiency statuses on
#'   attributes are independent of each other.
#' @param method Estimation method. Options are `"mcmc"`, which uses Stan's
#'   sampling method, or `"optim"`, which uses Stan's optimizer.
#' @param prior A [prior][dcmstan::prior()] object. If `NULL`, default priors
#'   are used, as specified by [dcmstan::default_dcm_priors()].
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
#' @concept Bayesian
#' @concept Stan
#'
#' @keywords internal
#'
#' @return A `measrdcm` object.
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
                      type = c("lcdm", "dina", "dino", "crum"),
                      max_interaction = Inf,
                      attribute_structure = c("unconstrained",
                                              "independent"),
                      method = c("mcmc", "optim"),
                      prior = NULL,
                      backend = getOption("measr.backend", "rstan"),
                      file = NULL,
                      file_refit = getOption("measr.file_refit", "never"),
                      ...) {
  lifecycle::deprecate_warn(
    "2.0.0",
    "measr_dcm()",
    details = "This is a limited version of dcm_estimate(); use it instead."
  )

  type <- rlang::arg_match(type)
  attribute_structure <- rlang::arg_match(attribute_structure)

  model_spec <- dcm_specify(qmatrix = qmatrix, identifier = item_id,
                            measurement_model = switch(type,
                              "lcdm" = dcmstan::lcdm(max_interaction),
                              "dina" = dcmstan::dina(),
                              "dino" = dcmstan::dino(),
                              "crum" = dcmstan::crum()
                            ),
                            structural_model = switch(attribute_structure,
                              "unconstrained" = dcmstan::unconstrained(),
                              "independent" = dcmstan::independent()
                            ),
                            priors = prior)

  model <- dcm_estimate(dcm_spec = model_spec,
                        data = data, missing = missing, identifier = resp_id,
                        method = method, backend = backend,
                        file = file, file_refit = file_refit, ...)

  return(model)
}
