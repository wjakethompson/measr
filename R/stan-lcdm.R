lcdm_script <- function(qmatrix, prior = NULL, strc = "unconstrained",
                        max_interaction = Inf, ...) {
  # data block -----
  data_block <- glue::glue(
    "data {{
      int<lower=1> I;                 // number of items
      int<lower=1> R;                 // number of respondents
      int<lower=1> N;                 // number of observations
      int<lower=1> C;                 // number of classes
      int<lower=1> A;                 // number of attributes
      int<lower=1,upper=I> ii[N];     // item for observation n
      int<lower=1,upper=R> rr[N];     // respondent for observation n
      int<lower=0,upper=1> y[N];      // score for observation n
      int<lower=1,upper=N> start[R];  // starting row for respondent R
      int<lower=1,upper=I> num[R];    // number of rows (items) for respondent R
      matrix[C,A] Alpha;              // attribute pattern for each class
      matrix[I,C] Xi;                 // class attribute mastery indicator
    }}"
  )

  # parameters block -----
  all_params <- get_parameters(qmatrix = qmatrix, item_id = NULL,
                               rename_att = TRUE, type = "lcdm",
                               attribute_structure = strc)
  strc_params <- all_params %>%
    dplyr::filter(.data$class == "structural")
  meas_params <- all_params %>%
    dplyr::filter(.data$class != "structural") %>%
    dplyr::mutate(parameter = dplyr::case_when(is.na(.data$attributes) ~
                                                 "intercept",
                                               TRUE ~ .data$attributes)) %>%
    dplyr::select("item_id", "parameter", param_name = "coef") %>%
    dplyr::mutate(
      param_level = dplyr::case_when(
        .data$parameter == "intercept" ~ 0,
        !grepl("__", .data$parameter) ~ 1,
        TRUE ~ sapply(gregexpr(pattern = "__", text = .data$parameter),
                      function(.x) length(attr(.x, "match.length"))) + 1
      ),
      atts = gsub("[^0-9|_]", "", .data$parameter),
      comp_atts = one_down_params(.data$atts, item = .data$item_id),
      num_comp = dplyr::case_when(
        comp_atts == "" ~ 0,
        TRUE ~ sapply(gregexpr(pattern = ",", text = .data$comp_atts),
                      function(.x) length(attr(.x, "match.length"))) + 1
      ),
      param_name = glue::glue("l{item_id}_{param_level}",
                              "{gsub(\"__\", \"\", atts)}"),
      constraint = dplyr::case_when(
        .data$param_level == 0 ~ glue::glue(""),
        .data$param_level == 1 ~ glue::glue("<lower=0>"),
        .data$param_level >= 2 ~ glue::glue("<lower=-1 * min([{comp_atts}])>")
      ),
      param_def = dplyr::case_when(
        .data$param_level == 0 ~ glue::glue("real {param_name};"),
        .data$param_level >= 1 ~ glue::glue("real{constraint} {param_name};")
      )
    ) %>%
    dplyr::filter(.data$param_level <= max_interaction)

  intercepts <- meas_params %>%
    dplyr::filter(.data$param_level == 0) %>%
    dplyr::pull(.data$param_def)
  main_effects <- meas_params %>%
    dplyr::filter(.data$param_level == 1) %>%
    dplyr::pull(.data$param_def)
  interactions <- meas_params %>%
    dplyr::filter(.data$param_level >= 2) %>%
    dplyr::pull(.data$param_def)

  interaction_stan <- if (length(interactions) > 0) {
    glue::glue(
      "",
      "",
      "  ////////////////////////////////// item interactions",
      "  {glue::glue_collapse(interactions, sep = \"\n  \")}",
      .sep = "\n", .trim = FALSE
    )
  } else {
    ""
  }

  strc_code <- strc_script(strc = strc)
  parameters_block <- glue::glue(
    "parameters {{",
    "{strc_code$parameters}",
    "",
    "  ////////////////////////////////// item intercepts",
    "  {glue::glue_collapse(intercepts, sep = \"\n  \")}",
    "",
    "  ////////////////////////////////// item main effects",
    "  {glue::glue_collapse(main_effects, sep = \"\n  \")}{interaction_stan}",
    "}}", .sep = "\n"
  )

  # transformed parameters block -----
  all_profiles <- create_profiles(attributes = ncol(qmatrix))

  profile_params <-
    stats::model.matrix(stats::as.formula(paste0("~ .^",
                                                 max(ncol(all_profiles),
                                                     2L))),
                        all_profiles) %>%
    tibble::as_tibble(.name_repair = model_matrix_name_repair) %>%
    tibble::rowid_to_column(var = "profile_id") %>%
    tidyr::pivot_longer(-"profile_id", names_to = "parameter",
                        values_to = "valid_for_profile")

  pi_def <- tidyr::expand_grid(item_id = unique(meas_params$item_id),
                               profile_id = seq_len(nrow(all_profiles))) %>%
    dplyr::left_join(dplyr::select(meas_params, "item_id", "parameter",
                                   "param_name"),
                     by = "item_id",
                     multiple = "all", relationship = "many-to-many") %>%
    dplyr::left_join(profile_params, by = c("profile_id", "parameter"),
                     relationship = "many-to-one") %>%
    dplyr::filter(.data$valid_for_profile == 1) %>%
    dplyr::group_by(.data$item_id, .data$profile_id) %>%
    dplyr::summarize(meas_params = paste(unique(.data$param_name),
                                        collapse = "+"),
                     .groups = "drop") %>%
    glue::glue_data("pi[{item_id},{profile_id}] = inv_logit({meas_params});")

  transformed_parameters_block <- glue::glue(
    "transformed parameters {{",
    "{strc_code$transformed}",
    "  matrix[I,C] pi;",
    "",
    "  ////////////////////////////////// probability of correct response",
    "  {glue::glue_collapse(pi_def, sep = \"\n  \")}",
    "}}", .sep = "\n"
  )

  # model block -----
  mod_prior <- if (is.null(prior)) {
    default_dcm_priors(type = "lcdm", attribute_structure = strc)
  } else {
    c(prior, default_dcm_priors(type = "lcdm", attribute_structure = strc),
      replace = TRUE)
  }

  if (max_interaction <= 1) {
    mod_prior <- dplyr::filter(mod_prior, .data$class != "interaction")
  }

  item_priors <- meas_params %>%
    dplyr::mutate(
      class = dplyr::case_when(.data$param_level == 0 ~ "intercept",
                               .data$param_level == 1 ~ "maineffect",
                               .data$param_level > 1 ~ "interaction")) %>%
    dplyr::left_join(mod_prior, by = c("class", "param_name" = "coef"),
                     relationship = "one-to-one") %>%
    dplyr::rename(coef_def = "prior_def") %>%
    dplyr::left_join(mod_prior %>%
                       dplyr::filter(is.na(.data$coef)) %>%
                       dplyr::select(-"coef"),
                     by = c("class"), relationship = "many-to-one") %>%
    dplyr::rename(class_def = "prior_def") %>%
    dplyr::mutate(
      prior = dplyr::case_when(!is.na(.data$coef_def) ~ .data$coef_def,
                               is.na(.data$coef_def) ~ .data$class_def),
      prior_def = glue::glue("{param_name} ~ {prior};")) %>%
    dplyr::pull("prior_def")

  strc_prior <- strc_params %>%
    dplyr::left_join(mod_prior, by = c("class", "coef"),
                     relationship = "one-to-one") %>%
    dplyr::rename(coef_def = "prior_def") %>%
    dplyr::left_join(mod_prior %>%
                       dplyr::filter(is.na(.data$coef)) %>%
                       dplyr::select(-"coef"),
                     by = c("class"), relationship = "many-to-one") %>%
    dplyr::rename(class_def = "prior_def") %>%
    dplyr::mutate(
      prior = dplyr::case_when(!is.na(.data$coef_def) ~ .data$coef_def,
                               is.na(.data$coef_def) ~ .data$class_def),
      prior_def = glue::glue("{coef} ~ {prior};")
    ) %>%
    dplyr::pull("prior_def")

  all_priors <- glue::as_glue(c(strc_prior, item_priors))

  model_block <- glue::glue(
    "model {{",
    "  real ps[C];",
    "",
    "  ////////////////////////////////// priors",
    "  {glue::glue_collapse(all_priors, sep = \"\n  \")}",
    "",
    "  ////////////////////////////////// likelihood",
    "  for (r in 1:R) {{",
    "    for (c in 1:C) {{",
    "      real log_items[num[r]];",
    "      for (m in 1:num[r]) {{",
    "        int i = ii[start[r] + m - 1];",
    "        log_items[m] = y[start[r] + m - 1] * log(pi[i,c]) +",
    "                       (1 - y[start[r] + m - 1]) * log(1 - pi[i,c]);",
    "      }}",
    "      ps[c] = log_Vc[c] + sum(log_items);",
    "    }}",
    "    target += log_sum_exp(ps);",
    "  }}",
    "}}", .sep = "\n"
  )

  # combine blocks -----
  full_script <- glue::glue(
    "{data_block}",
    "{parameters_block}",
    "{transformed_parameters_block}",
    "{model_block}",
    .sep = "\n"
  )

  return(list(stancode = full_script, prior = mod_prior))
}

crum_script <- function(qmatrix, prior = NULL, strc = "unconstrained", ...) {
  lcdm_script(qmatrix, prior, strc = strc, max_interaction = 1L)
}
