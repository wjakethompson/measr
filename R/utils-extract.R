# General extracts -------------------------------------------------------------
extract_m2 <- function(model) {
  if (is.null(model$fit$m2)) {
    rlang::abort(message = glue::glue("Model fit information must be ",
                                      "added to a model object before ",
                                      "the M2 can be extracted. See ",
                                      "`?add_fit()`."))
  }
  dplyr::select(model$fit$m2, "m2", "df", "pval")
}

extract_rmsea <- function(model) {
  if (is.null(model$fit$m2)) {
    rlang::abort(message = glue::glue("Model fit information must be ",
                                      "added to a model object before ",
                                      "the RMSEA can be extracted. See ",
                                      "`?add_fit()`."))
  }

  dplyr::select(model$fit$m2, "rmsea", dplyr::ends_with("CI"))
}

extract_srmsr <- function(model) {
  if (is.null(model$fit$m2)) {
    rlang::abort(message = glue::glue("Model fit information must be ",
                                      "added to a model object before ",
                                      "the SRMSR can be extracted. See ",
                                      "`?add_fit()`."))
  }

  dplyr::select(model$fit$m2, "srmsr")
}

extract_ppmc_raw_score <- function(model) {
  if (is.null(model$fit$ppmc$model_fit$raw_score)) {
    rlang::abort(message = glue::glue("Model fit information must be ",
                                      "added to a model object before ",
                                      "the raw score distribution can be ",
                                      "extracted. See `?add_fit()`."))
  }
  model$fit$ppmc$model_fit$raw_score
}

extract_or <- function(model, ppmc_interval = 0.95) {
  if (!is.null(ppmc_interval)) {
    ppmc_interval <- check_double(ppmc_interval, lb = 0, ub = 1,
                                  name = "ppmc_interval")
  }

  if (is.null(model$fit$ppmc$item_fit$odds_ratio)) {
    rlang::abort(message = glue::glue("Model fit information must be ",
                                      "added to a model object before ",
                                      "odds ratios can be extracted. See ",
                                      "`?add_fit()`."))
  }

  res <- if (is.null(ppmc_interval)) {
    model$fit$ppmc$item_fit$odds_ratio
  } else {
    model$fit$ppmc$item_fit$odds_ratio %>%
      dplyr::filter(!dplyr::between(.data$ppp,
                                    (1 - ppmc_interval) / 2,
                                    1 - ((1 - ppmc_interval) / 2)))
  }

  return(res)
}

extract_info_crit <- function(model, criterion) {
  if (is.null(model$criteria[[criterion]])) {
    rlang::abort(message = glue::glue("The {toupper(criterion)} criterion ",
                                      "must be added to a model object before ",
                                      "it can be extracted. See ",
                                      "`?add_criterion()`."))
  }
  model$criteria[[criterion]]
}

# DCM-specific extracts --------------------------------------------------------
dcm_extract_item_param <- function(model) {
  items <- model$data$qmatrix %>%
    dplyr::select(item = "item_id") %>%
    dplyr::mutate(item_id = as.integer(.data$item))
  params <- get_parameters(dplyr::select(model$data$qmatrix, -"item_id"),
                           type = model$type,
                           rename_item = TRUE) %>%
    dplyr::filter(.data$class != "structural") %>%
    dplyr::left_join(items, by = "item_id", multiple = "all") %>%
    dplyr::select("item", dplyr::everything(), -"item_id")
  draws <- as_draws(model) %>%
    posterior::subset_draws(variable = dplyr::pull(params, "coef")) %>%
    posterior::as_draws_rvars() %>%
    tibble::as_tibble()


  if (model$type %in% c("lcdm", "crum")) {
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
}

dcm_extract_strc_param <- function(model) {
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
}

dcm_extract_classes <- function(model) {
  create_profiles(ncol(model$data$qmatrix) - 1) %>%
    rlang::set_names(colnames(model$data$qmatrix)[-1]) %>%
    tibble::rowid_to_column(var = "class_id") %>%
    dplyr::left_join(profile_labels(ncol(model$data$qmatrix) - 1),
                     by = "class_id", relationship = "one-to-one") %>%
    dplyr::select("class", dplyr::everything(), -"class_id")
}

dcm_extract_class_prob <- function(model) {
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
}

dcm_extract_attr_prob <- function(model) {
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
}

dcm_extract_ppmc_cond_prob <- function(model, ppmc_interval = 0.95) {
  if (!is.null(ppmc_interval)) {
    ppmc_interval <- check_double(ppmc_interval, lb = 0, ub = 1,
                                  name = "ppmc_interval")
  }

  if (is.null(model$fit$ppmc$item_fit$conditional_prob)) {
    rlang::abort(message = glue::glue("Model fit information must be ",
                                      "added to a model object before ",
                                      "conditional probabilities can be ",
                                      "extracted. See `?add_fit()`."))
  }

  res <- if (is.null(ppmc_interval)) {
    model$fit$ppmc$item_fit$conditional_prob
  } else {
    model$fit$ppmc$item_fit$conditional_prob %>%
      dplyr::filter(!dplyr::between(.data$ppp,
                                    (1 - ppmc_interval) / 2,
                                    1 - ((1 - ppmc_interval) / 2)))
  }

  return(res)
}

dcm_extract_patt_reli <- function(model) {
  if (identical(model$reliability, list())) {
    rlang::abort(message = glue::glue("Reliability information must be ",
                                      "added to a model object before it ",
                                      "can be extracted. See ",
                                      "`?add_reliability()`."))
  }

  model$reliability$pattern_reliability %>%
    tibble::enframe() %>%
    tidyr::pivot_wider(names_from = "name", values_from = "value") %>%
    dplyr::rename(accuracy = "p_a", consistency = "p_c")
}

dcm_extract_map_reli <- function(model, agreement = NULL) {
  if (identical(model$reliability, list())) {
    rlang::abort(message = glue::glue("Reliability information must be ",
                                      "added to a model object before it ",
                                      "can be extracted. See ",
                                      "`?add_reliability()`."))
  }

  if (is.null(agreement)) {
    agreement <- c("acc", "consist")
  } else {
    agreement <- c("acc", "consist", agreement)
  }

  dplyr::full_join(
    dplyr::select(model$reliability$map_reliability$accuracy,
                  "attribute", dplyr::any_of(dplyr::matches(agreement))),
    dplyr::select(model$reliability$map_reliability$consistency,
                  "attribute", dplyr::any_of(dplyr::matches(agreement))),
    by = "attribute"
  ) %>%
    dplyr::rename(accuracy = "acc", consistency = "consist")
}

dcm_extract_eap_reli <- function(model, agreement = NULL) {
  if (identical(model$reliability, list())) {
    rlang::abort(message = glue::glue("Reliability information must be ",
                                      "added to a model object before it ",
                                      "can be extracted. See ",
                                      "`?add_reliability()`."))
  }

  if (is.null(agreement)) {
    agreement <- c("rho_i")
  } else {
    agreement <- c("rho_i", agreement)
  }

  dplyr::select(model$reliability$eap_reliability,
                "attribute", dplyr::any_of(dplyr::matches(agreement))) %>%
    dplyr::rename(informational = "rho_i")
}
