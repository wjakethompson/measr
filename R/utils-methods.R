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

  mastery <- mastery %>%
    tidyr::pivot_longer(cols = -"resp_id",
                        names_to = "class",
                        values_to = "probability")

  return(mastery)
}

extract_attr_probs <- function(model, qmat, method) {
  draws <- posterior::as_draws_rvars(model)

  mastery <- draws$prob_resp_attr %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~ colnames(qmat)) %>%
    tibble::rowid_to_column(var = "resp_id")

  mastery <- mastery %>%
    tidyr::pivot_longer(cols = -"resp_id",
                        names_to = "attribute",
                        values_to = "probability")

  return(mastery)
}

summarize_probs <- function(x, probs, id, optim) {
  type <- colnames(x)[!grepl(glue::glue("{id}|probability"), colnames(x))]

  sum_frame <- x %>%
    dplyr::mutate(bounds = lapply(.data$probability, posterior::rvar_quantile,
                                  probs = probs, names = TRUE),
                  bounds = lapply(.data$bounds, tibble::as_tibble_row)) %>%
    tidyr::unnest("bounds") %>%
    dplyr::mutate(dplyr::across(dplyr::where(posterior::is_rvar), E))

  if (optim) {
    sum_frame <- sum_frame %>%
      dplyr::select(!!id, !!type, "probability")
  }

  return(sum_frame)
}
