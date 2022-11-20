model_matrix_name_repair <- function(x) {
  x <- gsub("\\(|\\)", "", x)
  x <- gsub(":", "__", x)
  x <- tolower(x)

  return(x)
}

one_down_params <- function(x, item) {
  all_atts <- strsplit(x, split = "__")[[1]]
  if (length(all_atts) <= 1) return("")
  att_combos <- combn(all_atts, m = length(all_atts) - 1, simplify = FALSE)

  paste("l", item, "_", length(all_atts) - 1,
        sapply(att_combos, paste, collapse = ""), sep = "", collapse = ",")
}
one_down_params <- Vectorize(one_down_params, USE.NAMES = FALSE)


define_interactions <- function(param_level, param_info) {
  vector_def <- param_info %>%
    glue::glue_data("vector[{num_comp}] {gsub('l', 'v', param_name)} = ",
                    "[{comp_atts}]';")
  interaction_constrain <- param_info %>%
    glue::glue_data("real {param_name} = exp({param_name}_raw) - ",
                    "min({gsub('l', 'v', param_name)});")

  trans_par_code <- glue::glue(
    "  ////////////////////////////////// {param_level}-way interactions",
    "  {glue::glue_collapse(vector_def, sep = \"\n  \")}",
    "",
    "  ////////////////////////////////// constrain {param_level}-way",
    "  {glue::glue_collapse(interaction_constrain, sep = \"\n  \")}",
    .sep = "\n", .trim = FALSE
  )

  return(trans_par_code)
}
