dina_script <- function(qmatrix, prior = NULL) {
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
  parameters_block <- glue::glue(
    "parameters {{",
    "  simplex[C] Vc;                  // base rates of class membership",
    "",
    "  ////////////////////////////////// item parameters",
    "  real<lower=0,upper=1> slip[I];",
    "  real<lower=0,upper=1> guess[I];",
    "}}", .sep = "\n"
  )

  # transformed parameters block -----
  transformed_parameters_block <- glue::glue(
    "transformed parameters {{",
    "  vector[C] log_Vc = log(Vc);",
    "  matrix[I,C] pi;",
    "",
    "  for (i in 1:I) {{",
    "    for (c in 1:C) {{",
    "      pi[i,c] = ((1 - slip[i]) ^ Xi[i,c]) * (guess[i] ^ (1 - Xi[i,c]));",
    "    }}",
    "  }}",
    "}}", .sep = "\n"
  )

  # model block -----
  mod_prior <- if (is.null(prior)) {
    default_dcm_priors(type = "dina")
  } else {
    c(prior, default_dcm_priors(type = "dina"), replace = TRUE)
  }

  all_priors <- get_parameters(qmatrix = qmatrix, item_id = NULL,
                               rename_att = TRUE, type = "dina") %>%
    dplyr::left_join(mod_prior, by = c("class", "coef")) %>%
    dplyr::rename(coef_def = "prior_def") %>%
    dplyr::left_join(mod_prior %>%
                       dplyr::filter(is.na(.data$coef)) %>%
                       dplyr::select(-"coef"),
                     by = c("class")) %>%
    dplyr::rename(class_def = "prior_def") %>%
    dplyr::mutate(
      prior = dplyr::case_when(!is.na(.data$coef_def) ~ .data$coef_def,
                               is.na(.data$coef_def) ~ .data$class_def),
      prior_def = glue::glue("{coef} ~ {prior};")) %>%
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

dino_script <- dina_script
