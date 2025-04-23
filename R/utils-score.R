profile_labels <- function(model_spec) {
  dcmstan::create_profiles(model_spec) |>
    tibble::rowid_to_column(var = "class_id") |>
    tidyr::pivot_longer(cols = -"class_id") |>
    dplyr::summarize(
      class = paste0("[", paste(.data$value, collapse = ","), "]"),
      .by = "class_id"
    ) |>
    dplyr::arrange("class_id")
}

extract_class_probs <- function(model, gq) {
  draws <- posterior::as_draws_rvars(gq)

  draws$prob_resp_class |>
    tibble::as_tibble() |>
    dplyr::rename_with(
      ~ profile_labels(model_spec = model@model_spec)$class
    ) |>
    tibble::rowid_to_column(var = "resp_id")
}

extract_attr_probs <- function(model, gq) {
  draws <- posterior::as_draws_rvars(gq)

  draws$prob_resp_attr |>
    tibble::as_tibble() |>
    dplyr::rename_with(
      ~ names(model@model_spec@qmatrix_meta$attribute_names)
    ) |>
    tibble::rowid_to_column(var = "resp_id")
}

calculate_probs <- function(model, gq, resp_id) {
  class_probs <- extract_class_probs(model, gq) |>
    dplyr::mutate(
      resp_id = names(model@data$respondent_names)[.data$resp_id]
    ) |>
    dplyr::rename(!!resp_id := "resp_id")

  attr_probs <- extract_attr_probs(model, gq) |>
    dplyr::mutate(
      resp_id = names(model@data$respondent_names)[.data$resp_id]
    ) |>
    dplyr::rename(!!resp_id := "resp_id")

  list(class_probabilities = class_probs,
       attribute_probabilities = attr_probs)
}

calculate_probs_no_summary <- function(res_list, method) {
  if (!S7::S7_inherits(method, optim)) return(res_list)

  lapply(res_list,
         function(.x) {
           dplyr::mutate(.x, dplyr::across(dplyr::where(posterior::is_rvar),
                                           posterior::E))
         })
}

calculate_probs_summary <- function(res_list, probs, method, resp_id) {
  lapply(res_list, summarize_probs, probs = probs, method = method,
         id = resp_id)
}

summarize_probs <- function(res, probs, method, id) {
  summary_names <- colnames(res)[!grepl(glue::glue("{id}|chain|iteration|draw"),
                                        colnames(res))]
  type <- dplyr::if_else(all(grepl("\\[[0-1,]+\\]", summary_names)),
                         "class", "attribute")

  res |>
    dplyr::mutate(dplyr::across(dplyr::where(posterior::is_rvar),
                                ~lapply(.x, summarize_rvar, probs = probs,
                                        method = method))) |>
    tidyr::pivot_longer(cols = dplyr::all_of(summary_names),
                        names_to = type,
                        values_to = "summary") |>
    tidyr::unnest("summary")
}

summarize_rvar <- function(rv, probs, method) {
  if (S7::S7_inherits(method, optim)) {
    return(tibble::tibble(probability = posterior::E(rv)))
  }

  tibble::tibble(probability = posterior::E(rv),
                 bounds = tibble::as_tibble_row(
                   stats::quantile(rv, probs = probs, names = TRUE),
                   .name_repair = ~paste0(probs * 100, "%")
                 )) |>
    tidyr::unnest("bounds")
}
