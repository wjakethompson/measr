#' Extract components of a `measrfit` object.
#'
#' @param model The estimated to extract information from.
#' @param ... Additional arguments passed to specific methods.
#'
#' @export
measr_extract <- function(model, ...) {
  UseMethod("measr_extract")
}

#' Extract components of an estimated diagnostic classification model
#'
#' @param what Character string. The information to be extracted. See details
#'   for available options.
#' @param ppmc_interval The compatibility interval used for determining model
#'   fit flags to return (e.g., `what = "odds_ratio_flags"`). For example, a
#'   `ppmc_interval` of 0.95 will return any PPMCs where the posterior
#'   predictive *p*-value (ppp) is less than 0.025 or greater than 0.975.
#'
#' @details
#' For diagnostic classification models, we can extract the following
#' information:
#'   * `item_param`: The estimated item parameters. This shows the name of the
#'     parameter, the class of the parameter, and the estimated value.
#'   * `strc_param`: The estimated structural parameters. This is the base rate
#'     of membership in each class. This shows the class pattern and the
#'     estimated proportion of respondents in each class.
#'   * `prior`: The priors used when estimating the model.
#'   * `classes`: The possible classes or profile patterns. This will show the
#'     class label (i.e., the pattern of proficiency) and the attributes
#'     included in each class.
#'   * `class_prob`: The probability that each respondent belongs to class
#'     (i.e., has the given pattern of proficiency).
#'   * `attribute_prob`: The proficiency probability for each respondent and
#'     attribute.
#'   * `m2`: The \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} fit statistic,
#'     including RMSEA and SRMSR indices. See [fit_m2()] for details.
#'     Model fit information must first be added to the model using [add_fit()].
#'   * `ppmc_raw_score`: The observed and posterior predicted chi-square
#'     statistic for the raw score distribution. See [fit_ppmc()] for details.
#'     Model fit information must first be added to the model using [add_fit()].
#'   * `ppmc_conditional_prob`: The observed and posterior predicted conditional
#'     probabilities of each class providing a correct response to each item.
#'     See [fit_ppmc()] for details.
#'     Model fit information must first be added to the model using [add_fit()].
#'   * `ppmc_conditional_prob_flags`: A subset of the PPMC conditional
#'     probabilities where the _ppp_ is outside the specified `ppmc_interval`.
#'   * `odds_ratio`: The observed and posterior predicted odds ratios of each
#'     item pair. See [fit_ppmc()] for details.
#'     Model fit information must first be added to the model using [add_fit()].
#'   * `odds_ratio_flags`: A subset of the PPMC odds ratios where the _ppp_ is
#'     outside the specified `ppmc_interval`.
#'   * `classification_reliability`: The classification accuracy and consistency
#'     for each attribute, using the metrics described by Johnson & Sinharay
#'     (2018). Reliability information must first be added to the model using
#'     [add_reliability()].
#'
#' @return The extracted information. The specific structure will vary depending
#'   on what is being extracted, but usually the returned object is a
#'   [tibble][tibble::tibble-package] with the requested information.
#'
#' @describeIn measr_extract Extract components of an estimated diagnostic
#'   classification model.
#'
#' @references Johnson, M. S., & Sinharay, S. (2018). Measures of agreement to
#'   assess attribute-level classification accuracy and consistency for
#'   cognitive diagnostic assessments. *Journal of Educational Measurement,
#'   55*(4), 635-664. \doi{10.1111/jedm.12196}
#'
#' @export
#' @examplesIf measr_examples()
#' rstn_mdm_lcdm <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' extract(rstn_mdm_lcdm, "strc_param")
measr_extract.measrdcm <- function(model, what, ppmc_interval = 0.95, ...) {
  ppmc_interval <- check_double(ppmc_interval, lb = 0, ub = 1,
                                name = "ppmc_interval")

  out <- switch(
    what,
    item_param = dcm_extract_item_param(model),
    strc_param = dcm_extract_strc_param(model),
    prior = model$prior,
    classes = dcm_extract_classes(model),
    class_prob = dcm_extract_class_prob(model),
    attribute_prob = dcm_extract_attr_prob(model),
    m2 = extract_m2(model),
    ppmc_raw_score = extract_ppmc_raw_score(model),
    ppmc_conditional_prob = dcm_extract_ppmc_cond_prob(model,
                                                       ppmc_interval = NULL),
    ppmc_conditional_prob_flags = dcm_extract_ppmc_cond_prob(model,
                                                             ppmc_interval),
    ppmc_odds_ratio = extract_or(model, ppmc_interval = NULL),
    ppmc_odds_ratio_flags = extract_or(model, ppmc_interval),
    loo = extract_info_crit(model, "loo"),
    waic = extract_info_crit(model, "waic"),
    classification_reliability = dcm_extract_reli_conacc(model),
    rlang::abort(message = glue::glue("Cannot extract element `{what}`"))
  )

  return(out)
}
