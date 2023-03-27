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
#' @param quiet Logical. Should informational summaries and messages be
#'   suppressed? Default is `FALSE`.
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
#'     including RMSEA and SRMSR indices. Model fit information must first be
#'     added to the model using [add_fit()].
#'   * `odds_ratio`: The observed and posterior predicted odds ratios of each
#'     item pair. Model fit information must first be added to the model using
#'     [add_fit()].
#'   * `odds_ratio_flags`: A subset of the PPMC odds ratios where the _ppp_ is
#'     outside the specified `ppmc_interval`.
#'   * `classification_accuracy`: The classification accuracy and consistency
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
measr_extract.measrdcm <- function(model, what, ppmc_interval = 0.95,
                                   quiet = FALSE, ...) {
  ppmc_interval <- check_double(ppmc_interval, lb = 0, ub = 1,
                                name = "ppmc_interval")
  quiet <- check_logical(quiet, name = "quiet")

  out <- switch(
    what,
    item_param = {
      items <- model$data$qmatrix %>%
        dplyr::select(item = "item_id") %>%
        dplyr::mutate(item_id = as.integer(.data$item))
      params <- get_parameters(dplyr::select(model$data$qmatrix, -"item_id"),
                               type = model$type) %>%
        dplyr::left_join(items, by = "item_id", multiple = "all") %>%
        dplyr::select("item", dplyr::everything(), -"item_id")
      draws <- as_draws(model) %>%
        posterior::subset_draws(variable = dplyr::pull(params, "coef")) %>%
        posterior::as_draws_rvars() %>%
        tibble::as_tibble()


      if (model$type %in% c("lcdm")) {
        draws <- draws %>%
          tidyr::pivot_longer(cols = dplyr::everything(),
                              names_to = "coef", values_to = "estimate")
        dplyr::left_join(params, draws, by = c("coef")) %>%
          dplyr::rename(!!model$dat$item_id := "item")
      } else if (model$type %in% c("dina", "dino")) {
        draws <- draws %>%
          dplyr::mutate(item = items$item) %>%
          tidyr::pivot_longer(cols = -"item",
                              names_to = "coef", values_to = "estimate")

        dplyr::left_join(params, draws, by = c("item", "class" = "coef"),
                         relationship = "one-to-one") %>%
          dplyr::rename(!!model$dat$item_id := "item")
      }
    },
    strc_param = {
      profiles <- profile_labels(ncol(model$data$qmatrix) - 1)

      as_draws(model) %>%
        posterior::subset_draws(variable = "Vc") %>%
        posterior::as_draws_rvars() %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = "coef", values_to = "estimate") %>%
        tibble::rowid_to_column(var = "class_id") %>%
        dplyr::left_join(profiles, by = "class_id") %>%
        dplyr::select("class", "estimate")
    },
    prior = model$prior,
    classes = {
      create_profiles(ncol(model$data$qmatrix) - 1) %>%
        rlang::set_names(colnames(model$data$qmatrix)[-1]) %>%
        tibble::rowid_to_column(var = "class_id") %>%
        dplyr::left_join(profile_labels(ncol(model$data$qmatrix) - 1),
                         by = "class_id", relationship = "one-to-one") %>%
        dplyr::select("class", dplyr::everything(), -"class_id")
    },
    class_prob = {
      if (identical(model$respondent_estimates, list())) {
        rlang::abort(message = glue::glue("Respondent estimates must be ",
                                          "added to a model object before ",
                                          "class probabilities ",
                                          "can be extracted. See ",
                                          "`?add_respondent_estimates()`."))
      }
      model$respondent_estimates$class_probabilities %>%
        dplyr::select(!!model$data$resp_id, "class", "probability") %>%
        tidyr::pivot_wider(names_from = "class",
                           values_from = "probability")
    },
    attribute_prob = {
      if (identical(model$respondent_estimates, list())) {
        rlang::abort(message = glue::glue("Respondent estimates must be ",
                                          "added to a model object before ",
                                          "attribute probabilities ",
                                          "can be extracted. See ",
                                          "`?add_respondent_estimates()`."))
      }
      model$respondent_estimates$attribute_probabilities %>%
        dplyr::select(!!model$data$resp_id, "attribute", "probability") %>%
        tidyr::pivot_wider(names_from = "attribute",
                           values_from = "probability")
    },
    m2 = {
      if (is.null(model$fit$m2)) {
        rlang::abort(message = glue::glue("Model fit information must be ",
                                          "added to a model object before ",
                                          "the M2 can be extracted. See ",
                                          "`?add_fit()`."))
      }
      fit <- model$fit$m2 %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.double),
                                    ~round(.x, digits = 3)))
      if (!quiet) {
        print(glue::glue("M2 = {fit$m2}, df = {fit$df}, p = {fit$pval}
                       RMSEA = {fit$rmsea}, CI: [{fit$ci_lower},{fit$ci_upper}]
                       SRMSR = {fit$srmsr}"))
      }

      return(invisible(model$fit$m2))
    },
    odds_ratio = {
      if (is.null(model$fit$ppmc$item_fit$odds_ratio)) {
        rlang::abort(message = glue::glue("Model fit information must be ",
                                          "added to a model object before ",
                                          "odds ratios can be extracted. See ",
                                          "`?add_fit()`."))
      }
      model$fit$ppmc$item_fit$odds_ratio
    },
    odds_ratio_flags = {
      if (is.null(model$fit$ppmc$item_fit$odds_ratio)) {
        rlang::abort(message = glue::glue("Model fit information must be ",
                                          "added to a model object before ",
                                          "odds ratios can be extracted. See ",
                                          "`?add_fit()`."))
      }
      model$fit$ppmc$item_fit$odds_ratio %>%
        dplyr::filter(!dplyr::between(.data$ppp,
                                      (1 - ppmc_interval) / 2,
                                      1 - ((1 - ppmc_interval) / 2)))
    },
    classification_reliability = {
      if (identical(model$reliability, list())) {
        rlang::abort(message = glue::glue("Reliability information must be ",
                                          "added to a model object before it ",
                                          "can be extracted. See ",
                                          "`?add_reliability()`."))
      }

      dplyr::full_join(
        dplyr::select(model$reliability$map_reliability$accuracy,
                      "attribute", accuracy = "acc"),
        dplyr::select(model$reliability$map_reliability$consistency,
                      "attribute", consistency = "consist"),
        by = "attribute"
      )
    },
    rlang::abort(message = glue::glue("Cannot extract element `{what}`"))
  )

  return(out)
}
