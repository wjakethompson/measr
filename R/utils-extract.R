# General extracts -------------------------------------------------------------
extract_m2 <- function(model, call) {
  if (rlang::is_empty(model@fit$m2)) {
    cli::cli_abort(
      cli::format_message(c("Model fit information must be added ",
                            "to a model object before ",
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
      cli::format_message(c("Model fit information must be added ",
                            "to a model object before ",
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
      cli::format_message(c("Model fit information must be added ",
                            "to a model object before ",
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
    model@fit$ppmc_odds_ratio |>
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
dcm_extract_item_param <- function(model, call) {
  items <- tibble::enframe(model@model_spec@qmatrix_meta$item_names,
                           name = "item", value = "item_id")
  params <- dcmstan::get_parameters(
    model@model_spec@measurement_model,
    qmatrix = model@model_spec@qmatrix,
    attributes = model@model_spec@qmatrix_meta$attribute_names,
    items = model@model_spec@qmatrix_meta$item_names
  ) |>
    dplyr::rename(item = "item_id")
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
  }

  dplyr::left_join(params, draws, by = "coefficient",
                   relationship = "one-to-one") |>
    dplyr::rename(!!model@data$item_identifier := "item")
}

dcm_extract_strc_param <- function(model, call) {
  profiles <- profile_labels(model@model_spec)

  draws <- get_draws(model, vars = "Vc") |>
    posterior::as_draws_rvars() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "coef", values_to = "estimate") |>
    tibble::rowid_to_column(var = "class_id") |>
    dplyr::left_join(profiles, by = "class_id") |>
    dplyr::select("class", "estimate")

  if (S7::S7_inherits(model@method, optim)) {
    draws <- draws |>
      dplyr::mutate(dplyr::across(dplyr::where(posterior::is_rvar), E))
  }

  draws
}

dcm_extract_model_pvalues <- function(model, call) {
  profiles <- profile_labels(model@model_spec)

  draws <- get_draws(model, vars = "pi") |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) |>
    dplyr::summarize(value = posterior::rvar(.data$value), .by = "name") |>
    tidyr::separate_wider_regex(
      "name",
      patterns = c("pi\\[", item_id = "[0-9]*",
                   ",", class_id = "[0-9]*", "\\]")
    ) |>
    dplyr::mutate(item_id = as.integer(item_id),
                  class_id = as.integer(class_id),
                  item_id = names(model@data$item_names)[.data$item_id]) |>
    dplyr::left_join(profiles, by = "class_id") |>
    dplyr::select(!!model@data$item_identifier := "item_id",
                  "class", pi = "value") |>
    tidyr::pivot_wider(names_from = "class", values_from = "pi")

  w_pval <- draws |>
    tidyr::pivot_longer(cols = -model@data$item_identifier,
                        names_to = "class", values_to = "pi") |>
    dplyr::left_join(dcm_extract_strc_param(model, call = call),
                     by = "class", relationship = "many-to-one") |>
    dplyr::mutate(prod = .data$pi * .data$estimate) |>
    dplyr::mutate(dplyr::across(where(is.double), posterior::as_rvar)) |>
    dplyr::summarize(overall = posterior::rvar_sum(.data$prod) /
                       posterior::rvar_sum(.data$estimate),
                     .by = "item")

  pvals <- dplyr::full_join(draws, w_pval, by = model@data$item_identifier,
                            relationship = "one-to-one")

  if (S7::S7_inherits(model@method, optim)) {
    pvals <- pvals |>
      dplyr::mutate(dplyr::across(dplyr::where(posterior::is_rvar), E))
  }

  pvals
}

dcm_extract_pi_matrix <- function(model, call) {
  dcm_extract_model_pvalues(model, call = call) |>
    dplyr::select(-"overall")
}

dcm_extract_classes <- function(model, call) {
  dcmstan::create_profiles(model@model_spec) |>
    tibble::rowid_to_column(var = "class_id") |>
    dplyr::left_join(profile_labels(model@model_spec),
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
  model@respondent_estimates$class_probabilities |>
    dplyr::select(!!model@data$respondent_identifier, "class", "probability") |>
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
  model@respondent_estimates$attribute_probabilities |>
    dplyr::select(!!model@data$respondent_identifier, "attribute",
                  "probability") |>
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
    model@fit$ppmc_conditional_prob |>
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
    model@fit$ppmc_pvalue |>
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

  model@reliability$pattern_reliability |>
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
    dplyr::rename(accuracy = "acc", consistency = "consist") |>
    dplyr::rename_with(\(x) {
      x <- gsub("_a", "_accuracy", x)
      x <- gsub("_c", "_consistency", x)
    })
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

  dplyr::select(model@reliability$eap_reliability,
                "attribute", dplyr::any_of(dplyr::matches(agreement))) |>
    dplyr::rename(informational = "rho_i") |>
    dplyr::rename_with(\(x) {
      x <- gsub("rho_pf", "parallel_forms", x)
      x <- gsub("rho_bs", "point_biserial", x)
      x <- gsub("rho_tb", "templin_bradshaw", x)
    })
}
