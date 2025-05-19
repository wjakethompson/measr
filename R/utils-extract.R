# General extracts -------------------------------------------------------------
extract_m2 <- function(model, call) {
  if (rlang::is_empty(model@fit$m2)) {
    cli::cli_abort(
      cli::format_message(c("Model fit information must be ",
                            "added to a model object before ",
                            "the M2 can be extracted. See ",
                            "{.fun add_fit}.")),
      call = call
    )
  }
  dplyr::select(model@fit$m2, "m2", "df", "pval")
}

extract_rmsea <- function(model, call) {
  if (rlang::is_empty(model@fit$m2)) {
    cli::cli_abort(
      cli::format_message(c("Model fit information must be ",
                            "added to a model object before ",
                            "the RMSEA can be extracted. See ",
                            "{.fun add_fit}.")),
      call = call
    )
  }

  dplyr::select(model@fit$m2, "rmsea", dplyr::ends_with("CI"))
}

extract_srmsr <- function(model, call) {
  if (rlang::is_empty(model@fit$m2)) {
    cli::cli_abort(
      cli::format_message(c("Model fit information must be ",
                            "added to a model object before ",
                            "the SRMSR can be extracted. See ",
                            "{.fun add_fit}.")),
      call = call
    )
  }

  dplyr::select(model@fit$m2, "srmsr")
}

extract_ppmc_raw_score <- function(model, call) {
  if (rlang::is_empty(model@fit$ppmc_raw_score)) {
    cli::cli_abort(
      cli::format_message(c("Model fit information must be ",
                            "added to a model object before ",
                            "the raw score distribution can be extracted. See ",
                            "{.fun add_fit}.")),
      call = call
    )
  }
  model@fit$ppmc_raw_score
}

extract_or <- function(model, ppmc_interval = 0.95, call) {
  check_number_decimal(ppmc_interval, min = 0, max = 1, allow_null = TRUE,
                       call = call)
  if (rlang::is_empty(model@fit$ppmc_odds_ratio)) {
    cli::cli_abort(
      cli::format_message(c("Model fit information must be ",
                            "added to a model object before ",
                            "odds ratios can be extracted. See ",
                            "{.fun add_fit}.")),
      call = call
    )
  }

  res <- if (is.null(ppmc_interval)) {
    model@fit$ppmc_odds_ratio
  } else {
<<<<<<< HEAD
    model@fit$ppmc_odds_ratio |>
=======
    model$fit$ppmc$item_fit$odds_ratio |>
>>>>>>> b561f7d (switch to native pipe)
      dplyr::filter(!dplyr::between(.data$ppp,
                                    (1 - ppmc_interval) / 2,
                                    1 - ((1 - ppmc_interval) / 2)))
  }

  res
}

extract_info_crit <- function(model, criterion, call) {
  if (rlang::is_empty(model@criteria[[criterion]])) {
    cli::cli_abort(
      cli::format_message(c("The {toupper(criterion)} criterion ",
                            "must be added to a model object before ",
                            "it can be extracted. See ",
                            "{.fun add_criterion}.")),
      call = call
    )
  }

  model@criteria[[criterion]]
}


# DCM-specific extracts --------------------------------------------------------
<<<<<<< HEAD
dcm_extract_item_param <- function(model, call) {
  items <- tibble::enframe(model@model_spec@qmatrix_meta$item_names,
                           name = "item", value = "item_id")
  params <- dcmstan::get_parameters(model@model_spec@measurement_model,
                                    qmatrix = model@model_spec@qmatrix) |>
    dplyr::left_join(items, by = "item_id", multiple = "all") |>
    dplyr::select("item", dplyr::everything(), -"item_id")
  draws <- as_draws(model) |>
    posterior::subset_draws(variable = dplyr::pull(params, "coefficient")) |>
    posterior::as_draws_rvars() |>
    tibble::as_tibble()

  draws <- if (nrow(draws) > 1) {
    draws |>
      dplyr::mutate(item = items$item_id) |>
      tidyr::pivot_longer(cols = -"item",
                          names_to = "coefficient", values_to = "estimate") |>
      dplyr::mutate(
        coefficient = paste0(.data$coefficient, "[", .data$item, "]")
      ) |>
      dplyr::select(-"item")
  } else {
    draws |>
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "coefficient", values_to = "estimate")
=======
dcm_extract_item_param <- function(model) {
  items <- model$data$qmatrix |>
    dplyr::select(item = "item_id") |>
    dplyr::mutate(item_id = as.integer(.data$item))
  params <- get_parameters(dplyr::select(model$data$qmatrix, -"item_id"),
                           type = model$type,
                           rename_item = TRUE) |>
    dplyr::filter(.data$class != "structural") |>
    dplyr::left_join(items, by = "item_id", multiple = "all") |>
    dplyr::select("item", dplyr::everything(), -"item_id")
  draws <- as_draws(model) |>
    posterior::subset_draws(variable = dplyr::pull(params, "coef")) |>
    posterior::as_draws_rvars() |>
    tibble::as_tibble()


  if (model$type %in% c("lcdm", "crum")) {
    draws <- draws |>
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "coef", values_to = "estimate")
    dplyr::left_join(params, draws, by = c("coef")) |>
      dplyr::rename(!!model$dat$item_id := "item")
  } else if (model$type %in% c("dina", "dino")) {
    draws <- draws |>
      dplyr::mutate(item = items$item) |>
      tidyr::pivot_longer(cols = -"item",
                          names_to = "coef", values_to = "estimate")

    dplyr::left_join(params, draws, by = c("item", "class" = "coef"),
                     relationship = "one-to-one") |>
      dplyr::rename(!!model$dat$item_id := "item")
>>>>>>> b561f7d (switch to native pipe)
  }

  dplyr::left_join(params, draws, by = "coefficient",
                   relationship = "one-to-one") |>
    dplyr::rename(!!model@data$item_identifier := "item")
}

dcm_extract_strc_param <- function(model, call) {
  profiles <- profile_labels(model@model_spec)

<<<<<<< HEAD
  get_draws(model, vars = "Vc") |>
=======
  as_draws(model) |>
    posterior::subset_draws(variable = "Vc") |>
>>>>>>> b561f7d (switch to native pipe)
    posterior::as_draws_rvars() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "coef", values_to = "estimate") |>
    tibble::rowid_to_column(var = "class_id") |>
    dplyr::left_join(profiles, by = "class_id") |>
    dplyr::select("class", "estimate")
}

<<<<<<< HEAD
dcm_extract_classes <- function(model, call) {
  dcmstan::create_profiles(model@model_spec) |>
    tibble::rowid_to_column(var = "class_id") |>
    dplyr::left_join(profile_labels(model@model_spec),
=======
dcm_extract_classes <- function(model) {
  create_profiles(ncol(model$data$qmatrix) - 1) |>
    rlang::set_names(colnames(model$data$qmatrix)[-1]) |>
    tibble::rowid_to_column(var = "class_id") |>
    dplyr::left_join(profile_labels(ncol(model$data$qmatrix) - 1),
>>>>>>> b561f7d (switch to native pipe)
                     by = "class_id", relationship = "one-to-one") |>
    dplyr::select("class", dplyr::everything(), -"class_id")
}

dcm_extract_class_prob <- function(model, call) {
  if (rlang::is_empty(model@respondent_estimates)) {
    cli::cli_abort(
      cli::format_message(c("Respondent estimates must be ",
                            "added to a model object before ",
                            "class probabilities can be extracted. See ",
                            "{.fun add_respondent_estimates}.")),
      call = call
    )
  }
<<<<<<< HEAD
  model@respondent_estimates$class_probabilities |>
    dplyr::select(!!model@data$respondent_identifier, "class", "probability") |>
=======
  model$respondent_estimates$class_probabilities |>
    dplyr::select(!!model$data$resp_id, "class", "probability") |>
>>>>>>> b561f7d (switch to native pipe)
    tidyr::pivot_wider(names_from = "class",
                       values_from = "probability")
}

dcm_extract_attr_prob <- function(model, call) {
  if (rlang::is_empty(model@respondent_estimates)) {
    cli::cli_abort(
      cli::format_message(c("Respondent estimates must be ",
                            "added to a model object before ",
                            "attribute probabilities can be extracted. See ",
                            "{.fun add_respondent_estimates}.")),
      call = call
    )
  }
<<<<<<< HEAD
  model@respondent_estimates$attribute_probabilities |>
    dplyr::select(!!model@data$respondent_identifier, "attribute",
                  "probability") |>
=======
  model$respondent_estimates$attribute_probabilities |>
    dplyr::select(!!model$data$resp_id, "attribute", "probability") |>
>>>>>>> b561f7d (switch to native pipe)
    tidyr::pivot_wider(names_from = "attribute",
                       values_from = "probability")
}

dcm_extract_ppmc_cond_prob <- function(model, ppmc_interval = 0.95, call) {
  check_number_decimal(ppmc_interval, min = 0, max = 1, allow_null = TRUE,
                       call = call)

  if (rlang::is_empty(model@fit$ppmc_conditional_prob)) {
    cli::cli_abort(
      cli::format_message(c("Model fit information must be ",
                            "added to a model object before ",
                            "conditional probabilities can be extracted. See ",
                            "{.fun add_fit}.")),
      call = call
    )
  }

  res <- if (is.null(ppmc_interval)) {
    model@fit$ppmc_conditional_prob
  } else {
<<<<<<< HEAD
    model@fit$ppmc_conditional_prob |>
=======
    model$fit$ppmc$item_fit$conditional_prob |>
>>>>>>> b561f7d (switch to native pipe)
      dplyr::filter(!dplyr::between(.data$ppp,
                                    (1 - ppmc_interval) / 2,
                                    1 - ((1 - ppmc_interval) / 2)))
  }

  res
}

dcm_extract_ppmc_pvalue <- function(model, ppmc_interval = 0.95, call) {
  check_number_decimal(ppmc_interval, min = 0, max = 1, allow_null = TRUE,
                       call = call)

  if (rlang::is_empty(model@fit$ppmc_pvalue)) {
    cli::cli_abort(
      cli::format_message(c("Model fit information must be ",
                            "added to a model object before ",
                            "p-values can be extracted. See ",
                            "{.fun add_fit}.")),
      call = call
    )
  }

  res <- if (is.null(ppmc_interval)) {
    model@fit$ppmc_pvalue
  } else {
<<<<<<< HEAD
    model@fit$ppmc_pvalue |>
=======
    model$fit$ppmc$item_fit$pvalue |>
>>>>>>> b561f7d (switch to native pipe)
      dplyr::filter(!dplyr::between(.data$ppp,
                                    (1 - ppmc_interval) / 2,
                                    1 - ((1 - ppmc_interval) / 2)))
  }

  res
}

dcm_extract_patt_reli <- function(model, call) {
  if (rlang::is_empty(model@reliability)) {
    cli::cli_abort(
      cli::format_message(c("Reliability information must be ",
                            "added to a model object before ",
                            "it can be extracted. See ",
                            "{.fun add_reliability}.")),
      call = call
    )
  }

<<<<<<< HEAD
  model@reliability$pattern_reliability |>
=======
  model$reliability$pattern_reliability |>
>>>>>>> b561f7d (switch to native pipe)
    tibble::enframe() |>
    tidyr::pivot_wider(names_from = "name", values_from = "value") |>
    dplyr::rename(accuracy = "p_a", consistency = "p_c")
}

dcm_extract_map_reli <- function(model, agreement = NULL, call) {
  if (rlang::is_empty(model@reliability)) {
    cli::cli_abort(
      cli::format_message(c("Reliability information must be ",
                            "added to a model object before ",
                            "it can be extracted. See ",
                            "{.fun add_reliability}.")),
      call = call
    )
  }

  if (is.null(agreement)) {
    agreement <- c("acc", "consist")
  } else {
    agreement <- rlang::arg_match(
      agreement,
      values = c("lambda", "kappa", "youden", "tetra", "tp", "tn"),
      multiple = TRUE,
      error_call = call
    )
    agreement <- c("acc", "consist", agreement)
  }

  dplyr::full_join(
    dplyr::select(model@reliability$map_reliability$accuracy,
                  "attribute", dplyr::any_of(dplyr::matches(agreement))),
    dplyr::select(model@reliability$map_reliability$consistency,
                  "attribute", dplyr::any_of(dplyr::matches(agreement))),
    by = "attribute"
  ) |>
<<<<<<< HEAD
    dplyr::rename(accuracy = "acc", consistency = "consist") |>
    dplyr::rename_with(\(x) {
      x <- gsub("_a", "_accuracy", x)
      x <- gsub("_c", "_consistency", x)
    })
=======
    dplyr::rename(accuracy = "acc", consistency = "consist")
>>>>>>> b561f7d (switch to native pipe)
}

dcm_extract_eap_reli <- function(model, agreement = NULL, call) {
  if (rlang::is_empty(model@reliability)) {
    cli::cli_abort(
      cli::format_message(c("Reliability information must be ",
                            "added to a model object before ",
                            "it can be extracted. See ",
                            "{.fun add_reliability}.")),
      call = call
    )
  }

  if (rlang::is_empty(agreement)) {
    agreement <- c("rho_i")
  } else {
    agreement <- rlang::arg_match(
      agreement,
      values = c("pf", "bs", "tb"),
      multiple = TRUE,
      error_call = call
    )
    agreement <- c("rho_i", agreement)
  }

<<<<<<< HEAD
  dplyr::select(model@reliability$eap_reliability,
                "attribute", dplyr::any_of(dplyr::matches(agreement))) |>
    dplyr::rename(informational = "rho_i") |>
    dplyr::rename_with(\(x) {
      x <- gsub("rho_pf", "parallel_forms", x)
      x <- gsub("rho_bs", "point_biserial", x)
      x <- gsub("rho_tb", "templin_bradshaw", x)
    })
=======
  dplyr::select(model$reliability$eap_reliability,
                "attribute", dplyr::any_of(dplyr::matches(agreement))) |>
    dplyr::rename(informational = "rho_i")
>>>>>>> b561f7d (switch to native pipe)
}
