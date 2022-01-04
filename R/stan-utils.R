model_matrix_name_repair <- function(x) {
  x <- gsub("\\(|\\)", "", x)
  x <- gsub(":", "__", x)
  x <- tolower(x)

  return(x)
}

one_down_params <- function(x, item) {
  all_atts <- strsplit(x, split = "")[[1]]
  if (length(all_atts) <= 1) return("")
  att_combos <- combn(all_atts, m = length(all_atts) - 1, simplify = FALSE)

  paste("l", item, "_", length(all_atts) - 1,
        sapply(att_combos, paste, collapse = ""), sep = "", collapse = ",")
}
one_down_params <- Vectorize(one_down_params)
