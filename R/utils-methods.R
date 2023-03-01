get_mcmc_draws <- function(x) {
  draw_matrix <- if (x$backend == "cmdstanr") {
    if ("stanfit" %in% class(x$model)) {
      posterior::as_draws_array(as.array(x$model, pars = c("Vc", "pi")))
    } else {
      x$model$draws(variables = c("Vc", "pi"), format = "draws_array")
    }
  } else if (x$backend == "rstan") {
    as.matrix(x$model, pars = c("Vc", "pi"))
  }

  final_matrix <- fix_simplex(draw_matrix,
                              backend = x$backend,
                              method = x$method)
  return(final_matrix)
}

get_optim_draws <- function(x) {
  draw_matrix <- if (x$backend == "rstan") {
    t(as.matrix(x$model$par))
  } else if (x$backend == "cmdstanr") {
    as.matrix(x$model$draws())
  }

  all_vars <- colnames(draw_matrix)
  keep_vars <- all_vars[c(grep("^Vc", all_vars), # structural parameters
                          grep("^pi", all_vars))]

  final_matrix <- draw_matrix[, keep_vars]

  if (x$backend == "rstan") {
    final_matrix <- t(as.matrix(final_matrix))
  }

  final_matrix <- fix_simplex(final_matrix,
                              backend = x$backend,
                              method = x$method)
  return(final_matrix)
}

# ensure valid simplex (we love floating point rounding)
fix_simplex <- function(draws, backend, method) {
  if (method == "optim") {
    if (backend == "rstan") {
      simplex <- grep("^Vc", colnames(draws))
      draws[, simplex] <- draws[, simplex] /
        apply(t(as.matrix(draws[, simplex])), 1, sum)
    } else if (backend == "cmdstanr") {
      simplex <- grep("^Vc", colnames(draws))
      draws[, simplex] <- draws[, simplex] /
        apply(as.matrix(draws[, simplex]), 1, sum)
    }
  } else if (method == "mcmc") {
    if (backend == "rstan") {
      simplex <- grep("^Vc", colnames(draws))
      draws[, simplex] <- draws[, simplex] /
        apply(as.matrix(draws[, simplex]), 1, sum)
    } else if (backend == "cmdstanr") {
      simplex <- grep("^Vc", dimnames(draws)$variable)
      sum_matrix <- apply(draws[, , simplex], c(1, 2), sum)
      for (i in simplex) {
        draws[, , i] <- matrix(draws[, , i], ncol = dim(draws)[2]) /
          sum_matrix
      }
    }
  }

  return(draws)
}

extract_class_probs <- function(model, attr) {
  all_profiles <- create_profiles(attributes = attr) %>%
    tibble::rowid_to_column(var = "class_id") %>%
    tidyr::pivot_longer(cols = -"class_id") %>%
    dplyr::summarize(
      class = paste0("[", paste(.data$value, collapse = ","), "]"),
      .by = "class_id"
    ) %>%
    dplyr::arrange("class_id")

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

summarize_probs <- function(x, probs, id) {
  summary_names <- colnames(x)[!grepl(glue::glue("{id}|chain|iteration|draw"),
                                      colnames(x))]
  type <- dplyr::if_else(all(grepl("\\[[0-1,]+\\]", summary_names)),
                         "class", "attribute")

  x %>%
    dplyr::select(-c(".chain", ".iteration", ".draw")) %>%
    tidyr::pivot_longer(cols = -!!id, names_to = type, values_to = "prob") %>%
    dplyr::summarize(mean = mean(.data$prob, na.rm = TRUE),
                     bounds = list(
                       tibble::as_tibble_row(
                         stats::quantile(.data$prob, probs = probs,
                                         na.rm = TRUE)
                       )
                     ),
                     .by = c(!!id, !!type)) %>%
    tidyr::unnest("bounds")

}

prob_summary <- function(x, probs, na_rm) {
  x <- x$prob
  tibble::tibble(mean = mean(x, na.rm = na_rm),
                 bounds = list(
                   tibble::enframe(stats::quantile(x, probs = probs,
                                                   na.rm = na_rm)) %>%
                     tidyr::pivot_wider(names_from = "name",
                                        values_from = "value")
                 )) %>%
    tidyr::unnest("bounds")
}
