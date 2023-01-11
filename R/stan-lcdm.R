lcdm_script <- function(qmatrix, prior = NULL) {
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
  all_params <- stats::model.matrix(stats::as.formula(paste0("~ .^",
                                                             ncol(qmatrix))),
                                    qmatrix) %>%
    tibble::as_tibble(.name_repair = model_matrix_name_repair) %>%
    dplyr::select(where(~ sum(.x) > 0)) %>%
    tibble::rowid_to_column(var = "item_id") %>%
    tidyr::pivot_longer(cols = -"item_id", names_to = "parameter",
                        values_to = "value") %>%
    dplyr::filter(.data$value == 1) %>%
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
        .data$param_level == 1 ~ glue::glue("<lower=0>"),
        TRUE ~ glue::glue("")
      ),
      param_def = dplyr::case_when(
        .data$param_level == 0 ~ glue::glue("real {param_name};"),
        .data$param_level == 1 ~ glue::glue("real{constraint} {param_name};"),
        TRUE ~ glue::glue("real {param_name}_raw;")
      )
    )

  intercepts <- all_params %>%
    dplyr::filter(.data$param_level == 0) %>%
    dplyr::pull(.data$param_def)
  main_effects <- all_params %>%
    dplyr::filter(.data$param_level == 1) %>%
    dplyr::pull(.data$param_def)
  interactions <- all_params %>%
    dplyr::filter(.data$param_level >= 2) %>%
    dplyr::pull(.data$param_def)

  parameters_block <- glue::glue(
    "parameters {{",
    "  simplex[C] Vc;                  // base rates of class membership",
    "",
    "  ////////////////////////////////// item intercepts",
    "  {glue::glue_collapse(intercepts, sep = \"\n  \")}",
    "",
    "  ////////////////////////////////// item main effects",
    "  {glue::glue_collapse(main_effects, sep = \"\n  \")}",
    "",
    "  ////////////////////////////////// item interactions",
    "  {glue::glue_collapse(interactions, sep = \"\n  \")}",
    "}}", .sep = "\n"
  )

  # transformed parameters block -----
  vector_def <- all_params %>%
    dplyr::filter(.data$param_level >= 2) %>%
    glue::glue_data("vector[{num_comp}] {gsub('l', 'v', param_name)} = ",
                    "[{comp_atts}]';")
  raw_inter <- all_params %>%
    dplyr::filter(.data$param_level >= 2) %>%
    glue::glue_data("{param_name}_raw")
  interaction_constrain <- all_params %>%
    dplyr::filter(.data$param_level >= 2) %>%
    glue::glue_data("real {param_name} = exp({param_name}_raw) - ",
                    "min({gsub('l', 'v', param_name)});")

  all_profiles <- create_profiles(attributes = ncol(qmatrix))

  profile_params <-
    stats::model.matrix(stats::as.formula(paste0("~ .^",
                                                 ncol(all_profiles))),
                        all_profiles) %>%
    tibble::as_tibble(.name_repair = model_matrix_name_repair) %>%
    tibble::rowid_to_column(var = "profile_id") %>%
    tidyr::pivot_longer(-"profile_id", names_to = "parameter",
                        values_to = "valid_for_profile")

  pi_def <- tidyr::expand_grid(item_id = unique(all_params$item_id),
                               profile_id = seq_len(nrow(all_profiles))) %>%
    dplyr::left_join(dplyr::select(all_params, "item_id", "parameter",
                                   "param_name"),
                     by = "item_id",
                     multiple = "all") %>%
    dplyr::left_join(profile_params, by = c("profile_id", "parameter")) %>%
    dplyr::filter(.data$valid_for_profile == 1) %>%
    dplyr::group_by(.data$item_id, .data$profile_id) %>%
    dplyr::summarize(all_params = paste(unique(.data$param_name),
                                        collapse = "+"),
                     .groups = "drop") %>%
    glue::glue_data("pi[{item_id},{profile_id}] = inv_logit({all_params});")

  transformed_parameters_block <- glue::glue(
    "transformed parameters {{",
    "  vector[C] log_Vc = log(Vc);",
    "  matrix[I,C] pi;",
    "",
    "  ////////////////////////////////// vectors of interaction components",
    "  {glue::glue_collapse(vector_def, sep = \"\n  \")}",
    "",
    "  {glue::glue(\"real interaction_raw[{length(raw_inter)}] = \",
                   \"{{{glue::glue_collapse(raw_inter, sep = ',')}\")}}};",
    "",
    "  ////////////////////////////////// adjust to constrain interactions",
    "  {glue::glue_collapse(interaction_constrain, sep = \"\n  \")}",
    "",
    "  ////////////////////////////////// probability of correct response",
    "  {glue::glue_collapse(pi_def, sep = \"\n  \")}",
    "}}", .sep = "\n"
  )

  # model block -----
  mod_prior <- if (is.null(prior)) {
    default_dcm_priors(type = "lcdm")
  } else {
    c(prior, default_dcm_priors(type = "lcdm"), replace = TRUE)
  }

  all_priors <- all_params %>%
    dplyr::mutate(
      class = dplyr::case_when(.data$param_level == 0 ~ "intercept",
                               .data$param_level == 1 ~ "maineffect",
                               .data$param_level > 1 ~ "interaction")) %>%
    dplyr::left_join(mod_prior, by = c("class", "param_name" = "coef")) %>%
    dplyr::rename(coef_def = "prior_def") %>%
    dplyr::left_join(mod_prior %>%
                       dplyr::filter(is.na(.data$coef)) %>%
                       dplyr::select(-"coef"),
                     by = c("class")) %>%
    dplyr::rename(class_def = "prior_def") %>%
    dplyr::mutate(
      prior = dplyr::case_when(!is.na(.data$coef_def) ~ .data$coef_def,
                               is.na(.data$coef_def) ~ .data$class_def),
      prior_def = glue::glue("{param_name}",
                             "{ifelse(param_level >= 2, '_raw', '')} ",
                             "~ {prior};")) %>%
    dplyr::pull("prior_def")

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
    "",
    "  ////////////////////////////////// jacobian adjustment for constraints",
    "  for (i in 1:{length(raw_inter)}) {{",
    "    target += interaction_raw[i];",
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
