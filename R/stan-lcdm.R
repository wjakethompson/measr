lcdm_script <- function(qmatrix) {
  qmatrix <- check_qmatrix(qmatrix, name = "qmatrix")
  qmatrix <- dplyr::rename_with(qmatrix, ~glue::glue("att{1:ncol(qmatrix)}"))

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
      int<lower=1,upper=I> last[R];   // number of rows (items) for respondent R
      matrix[C,A] Alpha;              // attribute pattern for each class
    }}"
  )

  # parameters block -----
  all_params <- stats::model.matrix(stats::as.formula(paste0("~ .^",
                                                             ncol(qmatrix))),
                                    qmatrix) |>
    tibble::as_tibble(.name_repair = model_matrix_name_repair) |>
    dplyr::select(where(~ sum(.x) > 0)) |>
    tibble::rowid_to_column(var = "item_id") |>
    tidyr::pivot_longer(cols = -.data$item_id, names_to = "parameter",
                        values_to = "value") |>
    dplyr::filter(.data$value == 1) |>
    dplyr::mutate(
      param_level = dplyr::case_when(
        .data$parameter == "intercept" ~ 0,
        !grepl("__", .data$parameter) ~ 1,
        TRUE ~ sapply(gregexpr(pattern = "__", text = .data$parameter),
                      \(.x) length(attr(.x, "match.length"))) + 1
      ),
      atts = gsub("[^0-9]", "", .data$parameter),
      comp_atts = one_down_params(.data$atts, item = .data$item_id),
      num_comp = dplyr::case_when(
        comp_atts == "" ~ 0,
        TRUE ~ sapply(gregexpr(pattern = ",", text = .data$comp_atts),
                      \(.x) length(attr(.x, "match.length"))) + 1
      ),
      param_name = glue::glue("l{item_id}_{param_level}{atts}"),
      constraint = dplyr::case_when(
        .data$param_level == 0 ~ glue::glue(""),
        .data$param_level == 1 ~ glue::glue("<lower=0>"),
        TRUE ~ glue::glue("<lower=-1 * min(v{item_id}_{param_level})>")
      ),
      vector_def = dplyr::case_when(
        num_comp == 0 ~ glue::glue(""),
        TRUE ~ glue::glue("vector[{num_comp}] v{item_id}_{param_level} = [{comp_atts}];")
      ),
      param_def = dplyr::case_when(
        vector_def == "" ~ glue::glue("real{constraint} {param_name};"),
        TRUE ~ glue::glue("{vector_def}",
                          "real{constraint} {param_name};", .sep = ",,")
      )
    ) |>
    tidyr::separate_rows(.data$param_def, sep = ",,")

  intercepts <- all_params |>
    dplyr::filter(.data$param_level == 0) |>
    dplyr::pull(.data$param_def)
  main_effects <- all_params |>
    dplyr::filter(.data$param_level == 1) |>
    dplyr::pull(.data$param_def)
  interactions <- all_params |>
    dplyr::filter(.data$param_level >= 2) |>
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
  all_profiles <- create_profiles(attributes = ncol(qmatrix))

  transformed_parameters_block <- glue::glue(
    "transformed parameters {{",
    "  matrix[I,C] pi;",
  )
}
