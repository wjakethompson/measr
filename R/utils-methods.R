get_mcmc_draws <- function(x, ndraws = NULL) {
  draw_matrix <- if (x$backend == "cmdstanr") {
    x$model$draws(variables = c("log_Vc", "pi"), format = "draws_array")
  } else if (x$backend == "rstan") {
    posterior::as_draws_array(x$model) %>%
      posterior::subset_draws(variable = c("log_Vc", "pi"))
  }

  if (!is.null(ndraws)) {
    keep_draws <- sample(posterior::draw_ids(draw_matrix), size = ndraws,
                         replace = FALSE)
    draw_matrix <- posterior::subset_draws(posterior::merge_chains(draw_matrix),
                                           draw = keep_draws)
  }

  return(draw_matrix)
}

get_optim_draws <- function(x) {
  draw_matrix <- if (x$backend == "rstan") {
    posterior::as_draws_array(t(as.matrix(x$model$par)))
  } else if (x$backend == "cmdstanr") {
    posterior::as_draws_array(x$model$draws())
  }

  final_matrix <- posterior::subset_draws(draw_matrix,
                                          variable = c("log_Vc", "pi"))

  return(final_matrix)
}

extract_class_probs <- function(model, attr) {
  all_profiles <- profile_labels(attributes = attr)

  mastery <- posterior::as_draws_df(model) %>%
    tibble::as_tibble() %>%
    dplyr::select(".chain", ".iteration", ".draw",
                  dplyr::matches("prob_resp_class")) %>%
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) %>%
    tidyr::separate_wider_regex(
      cols = "name",
      patterns = c("prob_resp_class\\[", r = "\\d+", ",", c = "\\d+", "\\]")
    ) %>%
    dplyr::mutate(resp_id = as.integer(.data$r),
                  class_id = as.integer(.data$c)) %>%
    dplyr::left_join(all_profiles, by = "class_id") %>%
    dplyr::select(".chain", ".iteration", ".draw", "resp_id", "class",
                  probability = "value") %>%
    tidyr::pivot_wider(names_from = "class", values_from = "probability") %>%
    dplyr::select(".chain", ".iteration", ".draw", "resp_id",
                  dplyr::all_of(all_profiles$class))

  return(mastery)
}

extract_attr_probs <- function(model, qmat) {
  all_attributes <- colnames(qmat)

  mastery <- posterior::as_draws_df(model) %>%
    tibble::as_tibble() %>%
    dplyr::select(".chain", ".iteration", ".draw",
                  dplyr::matches("prob_resp_attr")) %>%
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) %>%
    tidyr::separate_wider_regex(
      cols = "name",
      patterns = c("prob_resp_attr\\[", r = "\\d+", ",", a = "\\d+", "\\]")
    ) %>%
    dplyr::mutate(resp_id = as.integer(.data$r),
                  attr = paste0("att", .data$a)) %>%
    dplyr::select(".chain", ".iteration", ".draw", "resp_id", "attr",
                  probability = "value") %>%
    tidyr::pivot_wider(names_from = "attr", values_from = "probability") %>%
    dplyr::select(".chain", ".iteration", ".draw", "resp_id",
                  dplyr::all_of(all_attributes))

  return(mastery)
}

summarize_probs <- function(x, probs, id, optim) {
  summary_names <- colnames(x)[!grepl(glue::glue("{id}|chain|iteration|draw"),
                                      colnames(x))]
  type <- dplyr::if_else(all(grepl("\\[[0-1,]+\\]", summary_names)),
                         "class", "attribute")

  sum_frame <- x %>%
    dplyr::select(-c(".chain", ".iteration", ".draw")) %>%
    tidyr::pivot_longer(cols = -!!id, names_to = type, values_to = "prob") %>%
    dplyr::summarize(probability = mean(.data$prob, na.rm = TRUE),
                     bounds = list(
                       tibble::as_tibble_row(
                         stats::quantile(.data$prob, probs = probs,
                                         na.rm = TRUE)
                       )
                     ),
                     .by = c(!!id, !!type)) %>%
    tidyr::unnest("bounds")

  if (optim) {
    sum_frame <- sum_frame %>%
      dplyr::select(!!id, !!type, "probability")
  }

  return(sum_frame)
}
