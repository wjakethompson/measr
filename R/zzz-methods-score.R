#' Posterior draws of respondent proficiency
#'
#' Calculate posterior draws of respondent proficiency. Optionally retain all
#' posterior draws or return only summaries of the distribution for each
#' respondent.
#'
#' @param x An estimated model (e.g., from [dcm_estimate()].
#' @param newdata Optional new data. If not provided, the data used to estimate
#'   the model is scored. If provided, `newdata` should be a data frame with 1
#'   row per respondent and 1 column per item. All items that appear in
#'   `newdata` should appear in the data used to estimate `x`.
#' @param missing An `R` expression specifying how missing data in `data` is
#'   coded (e.g., `NA`, `"."`, `-99`, etc.). The default is `NA`.
#' @param identifier Optional. Variable name of a column in `newdata` that
#'   contains respondent identifiers. `NULL` (the default) indicates that no
#'   identifiers are present in the data, and row numbers will be used as
#'   identifiers. If `newdata` is not specified and the data used to estimate
#'   the model is scored, the `resp_id` is taken from the original data.
#' @param summary Should summary statistics be returned instead of the raw
#'   posterior draws? Only relevant if the model was estimated with
#'   `method = "mcmc"`. Default is `FALSE`.
#' @param probs The percentiles to be computed by the [stats::quantile()]
#'   function. Only relevant if the model was estimated with `method = "mcmc"`.
#'   Only used if `summary` is `TRUE`.
#' @param force If respondent estimates have already been added to the model
#'   object with [add_respondent_estimates()], should they be recalculated.
#'   Default is `FALSE`.
#'
#' @return A list with two elements: `class_probabilities` and
#'   `attribute_probabilities`.
#'
#'   If summary is `FALSE`, each element is a tibble with one row per
#'   respondent. The columns include the respondent identifier, and one column
#'   of probabilities for each of the possible classes or attributes (as
#'   [posterior::rvar()] objects).
#'
#'   If summary is `TRUE`, each element is a tibble with one row per respondent
#'   and class or attribute. The columns include the respondent identifier,
#'   `class` or `attribute`, `mean`, and one column for every value specified in
#'   `probs`.
#' @export
score <- S7::new_generic(
  "score",
  "x",
  function(
    x,
    newdata = NULL,
    missing = NA,
    identifier = NULL,
    summary = TRUE,
    probs = c(0.025, 0.975),
    force = FALSE
  ) {
    S7::S7_dispatch()
  }
)

S7::method(score, measrdcm) <-
  function(
    x,
    newdata = NULL,
    missing = NA,
    identifier = NULL,
    summary = TRUE,
    probs = c(0.025, 0.975),
    force = FALSE
  ) {
    # check for existing scores ------------------------------------------------
    check_bool(force)
    check_bool(summary)
    if (!rlang::is_empty(x@respondent_estimates) && !force && summary) {
      return(x@respondent_estimates)
    }

    # check arguments ----------------------------------------------------------
    for (i in seq_along(probs)) {
      check_number_decimal(probs[i], min = 0, max = 1, arg = "probs")
    }

    if (!is.null(newdata)) {
      check_string(identifier, allow_null = TRUE)
      clean_data <- rdcmchecks::clean_data(
        newdata,
        identifier = identifier,
        missing = missing,
        cleaned_qmatrix = list(
          clean_qmatrix = x@model_spec@qmatrix,
          attribute_names = x@model_spec@qmatrix_meta$attribute_names,
          item_identifier = x@model_spec@qmatrix_meta$item_identifier,
          item_names = x@model_spec@qmatrix_meta$item_names
        ),
        valid_names = x@data$item_names,
        arg_qmatrix = "x"
      )
    } else {
      clean_data <- x@data
    }

    # run generated quantities -------------------------------------------------
    stan_data <- stan_data(x, clean_data = clean_data)
    stan_draws <- get_draws(x, vars = c("log_Vc", "pi"))
    stan_args <- default_stan_args(x@backend, gqs(), draws = stan_draws)
    stan_args$data <- stan_data

    stan_function_call <- stan_call(
      backend = x@backend,
      method = gqs(),
      code = dcmstan::stan_code(
        dcmstan::generated_quantities(probabilities = TRUE)
      ),
      args = stan_args,
      precompiled = stanmodels$gqs_probs
    )
    # fmt: skip
    out <- utils::capture.output( # nolint
      mod <- do.call(stan_function_call$call_function, stan_function_call$args)
    )

    # get mastery information --------------------------------------------------
    res_list <- calculate_probs(
      model = x,
      gq = mod,
      resp_id = clean_data$respondent_identifier
    )

    # return results -----------------------------------------------------------
    ret_list <- if (!summary) {
      calculate_probs_no_summary(res_list = res_list, method = x@method)
    } else {
      calculate_probs_summary(
        res_list = res_list,
        probs = probs,
        method = x@method,
        resp_id = clean_data$respondent_identifier
      )
    }

    ret_list
  }
