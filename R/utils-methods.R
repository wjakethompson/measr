constrain_01 <- function(x) {
  max(min(x, 0.99999), 0.00001)
}

constrain_pi <- function(draw_matrix) {
  posterior::bind_draws(
    posterior::subset_draws(draw_matrix, variable = "log_Vc"),
    apply(posterior::subset_draws(draw_matrix, variable = "pi"),
          c(1, 2, 3), constrain_01) %>%
      posterior::as_draws_array()
  )
}

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

  final_matrix <- constrain_pi(draw_matrix)

  return(final_matrix)
}

get_optim_draws <- function(x) {
  draw_matrix <- if (x$backend == "rstan") {
    posterior::as_draws_array(t(as.matrix(x$model$par)))
  } else if (x$backend == "cmdstanr") {
    posterior::as_draws_array(x$model$draws())
  }

  draw_matrix <- posterior::subset_draws(draw_matrix,
                                         variable = c("log_Vc", "pi"))
  final_matrix <- constrain_pi(draw_matrix)

  return(final_matrix)
}

extract_class_probs <- function(model, attr, method) {
  draws <- posterior::as_draws_rvars(model)

  mastery <- draws$prob_resp_class %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~ profile_labels(attributes = attr)$class) %>%
    tibble::rowid_to_column(var = "resp_id")

  return(mastery)
}

extract_attr_probs <- function(model, qmat, method) {
  draws <- posterior::as_draws_rvars(model)

  mastery <- draws$prob_resp_attr %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~ colnames(qmat)) %>%
    tibble::rowid_to_column(var = "resp_id")

  return(mastery)
}

calculate_probs <- function(model, qmat, method, resp_lookup, attr_lookup,
                            resp_id) {
  class_probs <- extract_class_probs(model = model,
                                     attr = ncol(qmat),
                                     method = method)
  attr_probs <- extract_attr_probs(model = model,
                                   qmat = qmat,
                                   method = method)

  class_probs <- class_probs %>%
    dplyr::left_join(resp_lookup, by = c("resp_id")) %>%
    dplyr::mutate(resp_id = .data$orig_resp) %>%
    dplyr::select(-"orig_resp") %>%
    dplyr::rename(!!resp_id := "resp_id")

  attr_probs <- attr_probs %>%
    dplyr::rename_with(~ c("resp_id", attr_lookup$real_names)) %>%
    dplyr::left_join(resp_lookup, by = c("resp_id")) %>%
    dplyr::mutate(resp_id = .data$orig_resp) %>%
    dplyr::select(-"orig_resp") %>%
    dplyr::rename(!!resp_id := "resp_id")

  ret_list <- list(class_probabilities = class_probs,
                   attribute_probabilities = attr_probs)

  return(ret_list)
}

summarize_probs <- function(x, probs, id, optim) {
  summary_names <- colnames(x)[!grepl(glue::glue("{id}|chain|iteration|draw"),
                                      colnames(x))]
  type <- dplyr::if_else(all(grepl("\\[[0-1,]+\\]", summary_names)),
                         "class", "attribute")

  sum_frame <- x %>%
    dplyr::mutate(dplyr::across(dplyr::where(posterior::is_rvar),
                                ~lapply(.x, summarize_rvar, probs = probs,
                                        optim = optim))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(summary_names),
                        names_to = type,
                        values_to = "summary") %>%
    tidyr::unnest("summary")

  return(sum_frame)
}

summarize_rvar <- function(rv, probs, optim) {
  ret_frame <- if (optim) {
    tibble::tibble(probability = E(rv))
  } else {
    tibble::tibble(probability = E(rv),
                   bounds = tibble::as_tibble_row(
                     stats::quantile(rv, probs = probs, names = TRUE),
                     .name_repair = ~paste0(probs * 100, "%")
                   )) %>%
      tidyr::unnest("bounds")
  }

  return(ret_frame)
}

calculate_probs_no_summary <- function(ret_list, method) {
  if (method == "optim") {
    ret_list <- lapply(ret_list,
                       function(.x) {
                         dplyr::mutate(
                           .x,
                           dplyr::across(dplyr::where(posterior::is_rvar),
                                         posterior::E)
                         )
                       })
    return(ret_list)
  } else {
    return(ret_list)
  }
}

calculate_probs_summary <- function(ret_list, probs, id, method) {
  summary_list <- lapply(ret_list, summarize_probs, probs = probs,
                         id = id,
                         optim = method == "optim")
  return(summary_list)
}
