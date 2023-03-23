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
measr_extract.measrdcm <- function(model, what, ...) {
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
      preds <- predict(model)
      preds$class_probabilities %>%
        dplyr::select(!!model$data$resp_id, "class", "mean") %>%
        tidyr::pivot_wider(names_from = "class", values_from = "mean")
    },
    attribute_prob = {
      preds <- predict(model)
      preds$attribute_probabilities %>%
        dplyr::select(!!model$data$resp_id, "attribute", "mean") %>%
        tidyr::pivot_wider(names_from = "attribute", values_from = "mean")
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
