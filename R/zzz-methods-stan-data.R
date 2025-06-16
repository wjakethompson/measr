#' @importFrom dcmstan stan_data
S7::method(stan_data, measrdcm) <- function(x, clean_data = NULL) {
  # default data list ----------------------------------------------------------
  if (is.null(clean_data)) {
    clean_data <- x@data
  }

  ragged_array <- clean_data$clean_data |>
    tibble::rowid_to_column() |>
    dplyr::group_by(.data$resp_id) |>
    dplyr::summarize(start = min(.data$rowid),
                     num = dplyr::n()) |>
    dplyr::arrange(.data$resp_id)

  profiles <- dcmstan::create_profiles(
    x@model_spec@structural_model,
    attributes = x@model_spec@qmatrix_meta$attribute_names
  )

  stan_data <- list(
    I = nrow(x@model_spec@qmatrix),
    R = length(clean_data$respondent_names),
    N = nrow(clean_data$clean_data),
    C = nrow(profiles),
    A = ncol(x@model_spec@qmatrix),
    ii = as.numeric(clean_data$clean_data$item_id),
    rr = as.numeric(clean_data$clean_data$resp_id),
    y = clean_data$clean_data$score,
    start = ragged_array$start,
    num = ragged_array$num,
    Alpha = as.matrix(profiles)
  )

  # return data ----------------------------------------------------------------
  stan_data
}
