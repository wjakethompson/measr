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
#' @param ... Additional arguments passed to each extract method.
#'   * `ppmc_interval`:
#'
#'     For `what = "odds_ratio_flags"` and
#'     `what = "conditional_prob_flags"`, the compatibility interval used for
#'     determining model fit flags to return. For example, a `ppmc_interval` of
#'     0.95 (the default) will return any PPMCs where the posterior predictive
#'     *p*-value (ppp) is less than 0.025 or greater than 0.975.
#'
#'   * `agreement`:
#'
#'     For `what = "classification_reliability"`, additional
#'     measures of agreement to include. By default, the classification
#'     accuracy and consistency metrics defined Johnson & Sinharay (2018) are
#'     returned. Additional metrics that can be specified to `agreement` are
#'     Goodman & Kruskal's lambda (`lambda`), Cohen's kappa (`kappa`), Youden's
#'     statistic (`youden`), the tetrachoric correlation (`tetra`), true
#'     positive rate (`tp`), and the true negative rate (`tn`).
#'
#'     For `what = "probability_reliability"`, additional measures of agreement
#'     to include. By default, the informational reliability index defined by
#'     Johnson & Sinharay (2020) is returned. Additional metrics that can be
#'     specified to `agreement` are the point biserial reliability index (`bs`),
#'     parallel forms reliability index (`pf`), and the tetrachoric reliability
#'     index (`tb`), which was originally defined by Templin & Bradshaw (2013).
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
#'   * `m2`: The \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} fit statistic.
#'     See [fit_m2()] for details. Model fit information must first be added to
#'     the model using [add_fit()].
#'   * `rmsea`: The root mean square error of approximation (RMSEA) fit
#'     statistic and associated confidence interval. See [fit_m2()] for details.
#'     Model fit information must first be added to the model using [add_fit()].
#'   * `srmsr`: The standardized root mean square residual (SRMSR) fit
#'     statistic. See [fit_m2()] for details. Model fit information must first
#'     be added to the model using [add_fit()].
#'   * `ppmc_raw_score`: The observed and posterior predicted chi-square
#'     statistic for the raw score distribution. See [fit_ppmc()] for details.
#'     Model fit information must first be added to the model using [add_fit()].
#'   * `ppmc_conditional_prob`: The observed and posterior predicted conditional
#'     probabilities of each class providing a correct response to each item.
#'     See [fit_ppmc()] for details.
#'     Model fit information must first be added to the model using [add_fit()].
#'   * `ppmc_conditional_prob_flags`: A subset of the PPMC conditional
#'     probabilities where the _ppp_ is outside the specified `ppmc_interval`.
#'   * `ppmc_odds_ratio`: The observed and posterior predicted odds ratios of
#'     each item pair. See [fit_ppmc()] for details.
#'     Model fit information must first be added to the model using [add_fit()].
#'   * `ppmc_odds_ratio_flags`: A subset of the PPMC odds ratios where the _ppp_
#'     is outside the specified `ppmc_interval`.
#'   * `loo`: The leave-one-out cross validation results. See [loo::loo()] for
#'     details. The information criterion must first be added to the model using
#'     [add_criterion()].
#'   * `waic`: The widely applicable information criterion results. See
#'     [loo::waic()] for details. The information criterion must first be added
#'     to the model using [add_criterion()].
#'   * `pattern_reliability`: The accuracy and consistency of the overall
#'     attribute profile classification, as described by Cui et al. (2012).
#'     Reliability information must first be added to the model using
#'     [add_reliability()].
#'   * `classification_reliability`: The classification accuracy and consistency
#'     for each attribute, using the metrics described by Johnson & Sinharay
#'     (2018). Reliability information must first be added to the model using
#'     [add_reliability()].
#'   * `probability_reliability`: Reliability estimates for the probability of
#'     proficiency on each attribute, as described by Johnson & Sinharay (2020).
#'     Reliability information must first be added to the model using
#'     [add_reliability()].
#'
#' @return The extracted information. The specific structure will vary depending
#'   on what is being extracted, but usually the returned object is a
#'   [tibble][tibble::tibble-package] with the requested information.
#'
#' @describeIn measr_extract Extract components of an estimated diagnostic
#'   classification model.
#'
#' @references Cui, Y., Gierl, M. J., & Chang, H.-H. (2012). Estimating
#'   classification consistency and accuracy for cognitive diagnostic
#'   assessment. *Journal of Educational Measurement, 49*(1), 19-38.
#'   \doi{10.1111/j.1745-3984.2011.00158.x}
#' @references Johnson, M. S., & Sinharay, S. (2018). Measures of agreement to
#'   assess attribute-level classification accuracy and consistency for
#'   cognitive diagnostic assessments. *Journal of Educational Measurement,
#'   55*(4), 635-664. \doi{10.1111/jedm.12196}
#' @references Johnson, M. S., & Sinharay, S. (2020). The reliability of the
#'   posterior probability of skill attainment in diagnostic classification
#'   models. *Journal of Educational and Behavioral Statistics, 45*(1), 5-31.
#'   \doi{10.3102/1076998619864550}
#' @references Templin, J., & Bradshaw, L. (2013). Measuring the reliability of
#'   diagnostic classification model examinee estimates. *Journal of
#'   Classification, 30*(2), 251-275. \doi{10.1007/s00357-013-9129-4}
#'
#' @export
#' @examplesIf measr_examples()
#' rstn_mdm_lcdm <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' measr_extract(rstn_mdm_lcdm, "strc_param")
measr_extract.measrdcm <- function(model, what, ...) {
  out <- switch(
    what,
    item_param = dcm_extract_item_param(model),
    strc_param = dcm_extract_strc_param(model),
    prior = model$prior,
    classes = dcm_extract_classes(model),
    class_prob = dcm_extract_class_prob(model),
    attribute_prob = dcm_extract_attr_prob(model),
    m2 = extract_m2(model),
    rmsea = extract_rmsea(model),
    srmsr = extract_srmsr(model),
    ppmc_raw_score = extract_ppmc_raw_score(model),
    ppmc_conditional_prob = dcm_extract_ppmc_cond_prob(model,
                                                       ppmc_interval = NULL),
    ppmc_conditional_prob_flags = dcm_extract_ppmc_cond_prob(model, ...),
    ppmc_odds_ratio = extract_or(model, ppmc_interval = NULL),
    ppmc_odds_ratio_flags = extract_or(model, ...),
    loo = extract_info_crit(model, "loo"),
    waic = extract_info_crit(model, "waic"),
    pattern_reliability = dcm_extract_patt_reli(model),
    classification_reliability = dcm_extract_map_reli(model, ...),
    probability_reliability = dcm_extract_eap_reli(model, ...),
    rlang::abort(message = glue::glue("Cannot extract element `{what}`"))
  )

  return(out)
}
