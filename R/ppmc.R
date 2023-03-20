#' @export
fit_ppmc <- function(model, ndraws = NULL, probs = c(0.025, 0.975),
                     return_draws = 0,
                     model_fit = c("raw_score"),
                     item_fit = c("conditional_prob", "odds_ratio")) {
  model <- check_model(model, required_class = "measrdcm", name = "object")
  total_draws <- posterior::ndraws(posterior::as_draws(model))
  ndraws <- check_integer(ndraws, lb = 1, ub = total_draws,
                          name = "ndraws", allow_null = TRUE)
  probs <- check_double(probs, lb = 0, ub = 1, inclusive = TRUE, name = "probs")
  return_draws <- check_double(return_draws, lb = 0, ub = 1, inclusive = TRUE,
                               name = "return_draws")
  model_fit <- if (!is.null(model_fit)) {
    rlang::arg_match(model_fit, multiple = TRUE)
  }
  item_fit <- if (!is.null(item_fit)) {
    rlang::arg_match(item_fit, multiple = TRUE)
  }

  clean_qmatrix <- model$data$qmatrix %>%
    dplyr::select(-"item_id") %>%
    dplyr::rename_with(~glue::glue("att{1:(ncol(model$data$qmatrix) - 1)}"))
  stan_data <- create_stan_data(dat = model$data$data, qmat = clean_qmatrix,
                                type = model$type)

  stan_draws <- get_mcmc_draws(model, ndraws = ndraws)

  stan_pars <- create_stan_gqs_params(backend = model$backend,
                                      draws = stan_draws)
  stan_pars$data <- stan_data

  # compile model -----
  stan_mod <- create_stan_function(backend = model$backend,
                                   method = "gqs",
                                   code = gqs_script(full_data = TRUE),
                                   pars = stan_pars,
                                   silent = 2)
  out <- utils::capture.output( #nolint
    gqs_model <- do.call(stan_mod$func, stan_mod$pars)
  )
  all_draws <- posterior::as_draws_array(gqs_model)

  # organize ppmc data
  data_posterior <- posterior::subset_draws(all_draws, variable = "y_rep") %>%
    posterior::as_draws_df() %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) %>%
    tidyr::separate_wider_regex(
      cols = "name",
      patterns = c("y_rep\\[", obs = "\\d+", "\\]")) %>%
    dplyr::mutate(obs = as.integer(.data$obs)) %>%
    dplyr::left_join(model$data$data %>%
                       dplyr::mutate(obs = seq_len(dplyr::n()),
                                     resp = as.integer(.data$resp_id),
                                     item = as.integer(.data$item_id)) %>%
                       dplyr::select("obs", "resp", "item"),
                     by = "obs")

  # calculate raw score distribution
  model_level_fit <- if (!is.null(model_fit)) {
    ppmc_model_fit(model = model,
                   post_data = data_posterior,
                   probs = probs,
                   return_draws = return_draws,
                   type = model_fit)
  } else {
    NULL
  }

  item_level_fit <- if (!is.null(item_fit)) {
    resp_prob <- extract_class_probs(model = gqs_model,
                                     attr = ncol(clean_qmatrix))

    ppmc_item_fit(model = model,
                  post_data = data_posterior,
                  attr = ncol(clean_qmatrix),
                  resp_prob = resp_prob, probs = probs,
                  return_draws = return_draws,
                  type = item_fit)
  } else {
    NULL
  }

  ret_list <- list(model_fit = model_level_fit,
                   item_fit = item_level_fit)
  ret_list[sapply(ret_list, is.null)] <- NULL

  return(ret_list)
}


ppmc_model_fit <- function(model, post_data, probs, return_draws, type) {
  raw_score <- if ("raw_score" %in% type) {
    ppmc_rawscore_chisq(model = model, post_data = post_data,
                        probs = probs, return_draws = return_draws)
  } else {
    NULL
  }

  mod_res <- list(raw_score = raw_score)
  mod_res[sapply(mod_res, is.null)] <- NULL

  return(mod_res)
}

ppmc_rawscore_chisq <- function(model, post_data, probs, return_draws) {
  raw_score_post <- post_data %>%
    dplyr::summarize(raw_score = sum(.data$value), .by = c("resp", ".draw")) %>%
    dplyr::count(.data$.draw, .data$raw_score) %>%
    tidyr::complete(.data$.draw, raw_score = 0:nrow(model$data$qmatrix),
                    fill = list(n = 0L))

  exp_raw_scores <- raw_score_post %>%
    dplyr::summarize(exp_resp = mean(.data$n), .by = "raw_score") %>%
    dplyr::mutate(exp_resp = sapply(.data$exp_resp,
                                    function(.x) max(.x, 0.0001)))

  chisq_ppmc <- raw_score_post %>%
    dplyr::left_join(exp_raw_scores, by = c("raw_score")) %>%
    dplyr::mutate(piece = ((.data$n - .data$exp_resp) ^ 2) / .data$exp_resp) %>%
    dplyr::summarize(chisq = sum(.data$piece), .by = ".draw")

  chisq_obs <- model$data$data %>%
    dplyr::summarize(raw_score = sum(.data$score), .by = "resp_id") %>%
    dplyr::count(.data$raw_score) %>%
    tidyr::complete(raw_score = 0:nrow(model$data$qmatrix),
                    fill = list(n = 0L)) %>%
    dplyr::left_join(exp_raw_scores, by = "raw_score") %>%
    dplyr::mutate(piece = ((.data$n - .data$exp_resp) ^ 2) / .data$exp_resp) %>%
    dplyr::summarize(chisq = sum(.data$piece)) %>%
    dplyr::pull("chisq")

  raw_score_res <- tibble::tibble(obs_chisq = chisq_obs,
                                  ppmc_mean = mean(chisq_ppmc$chisq),
                                  bounds = list(
                                    tibble::as_tibble_row(
                                      stats::quantile(chisq_ppmc$chisq,
                                                      probs = probs,
                                                      na.rm = TRUE)
                                    )
                                  ),
                                  ppp = mean(chisq_ppmc$chisq > chisq_obs)) %>%
    tidyr::unnest("bounds")

  if (return_draws > 0) {
    raw_score_res <- raw_score_res %>%
      dplyr::mutate(
        samples = list(chisq_ppmc %>%
                         dplyr::slice_sample(prop = return_draws) %>%
                         dplyr::pull("chisq")),
        .before = "ppp")
  }

  return(raw_score_res)
}

ppmc_item_fit <- function(model, post_data, attr, resp_prob, probs,
                          return_draws, type) {
  cond_prob <- if ("conditional_prob" %in% type) {
    ppmc_conditional_probs(model = model, attr = attr, resp_prob = resp_prob,
                           probs = probs, return_draws = return_draws)
  } else {
    NULL
  }
  odds_ratio <- if ("odds_ratio" %in% type) {
    ppmc_odds_ratio(model = model, post_data = post_data, probs, return_draws)
  } else {
    NULL
  }

  item_res <- list(conditional_prob = cond_prob,
                   odds_ratio = odds_ratio)
  item_res[sapply(item_res, is.null)] <- NULL

  return(item_res)
}

ppmc_conditional_probs <- function(model, attr, resp_prob, probs,
                                   return_draws) {
  all_profiles <- create_profiles(attributes = attr) %>%
    tibble::rowid_to_column(var = "class_id") %>%
    tidyr::pivot_longer(cols = -"class_id") %>%
    dplyr::summarize(
      class = paste0("[", paste(.data$value, collapse = ","), "]"),
      .by = "class_id"
    ) %>%
    dplyr::arrange("class_id")

  obs_class <- resp_prob %>%
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw", "resp_id"),
                        names_to = "class_label", values_to = "prob") %>%
    dplyr::mutate(max_class = .data$prob == max(.data$prob),
                  .by = c(".draw", "resp_id")) %>%
    dplyr::filter(.data$max_class) %>%
    dplyr::group_by(.data$.draw, .data$resp_id) %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(all_profiles, by = c("class_label" = "class")) %>%
    dplyr::select(".draw", "resp_id", class = "class_id")

  obs_cond_pval <- model$data$data %>%
    dplyr::mutate(resp_id = as.integer(.data$resp_id),
                  item_id = as.integer(.data$item_id)) %>%
    dplyr::left_join(obs_class, by = "resp_id", multiple = "all") %>%
    dplyr::summarize(obs_cond_pval = mean(.data$score),
                     .by = c("item_id", "class")) %>%
    dplyr::arrange(.data$item_id, .data$class)


  cond_pval_res <- tidybayes::spread_draws(
    model,
    (!!rlang::sym("pi"))[!!rlang::sym("i"),!!rlang::sym("c")], #nolint
    ndraws = dplyr::n_distinct(resp_prob$.draw)
  ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(".chain", ".iteration", ".draw")) %>%
    tidyr::nest(cond_pval = "pi") %>%
    dplyr::left_join(obs_cond_pval, by = c("i" = "item_id",
                                           "c" = "class")) %>%
    dplyr::mutate(ppmc_mean = vapply(.data$cond_pval, function(.x) mean(.x$pi),
                                     double(1)),
                  bounds = lapply(.data$cond_pval,
                                  function(.x, probs) {
                                    tibble::as_tibble_row(
                                      stats::quantile(.x$pi,
                                                      probs = probs,
                                                      na.rm = TRUE)
                                    )
                                  },
                                  probs = probs),
                  ppp = mapply(function(exp, obs) {
                                 mean(exp$pi > obs)
                               },
                               .data$cond_pval, .data$obs_cond_pval)) %>%
    tidyr::unnest("bounds")

  if (return_draws > 0) {
    cond_pval_res <- cond_pval_res %>%
      dplyr::mutate(
        samples = lapply(.data$cond_pval,
                         function(.x) {
                           .x %>%
                             dplyr::slice_sample(prop = return_draws) %>%
                             dplyr::pull("pi")
                         }),
        .before = "ppp")
  }

  cond_pval_res <- cond_pval_res %>%
    dplyr::select(-"cond_pval") %>%
    dplyr::rename(item_id = "i", class_id = "c")

  return(cond_pval_res)
}

ppmc_odds_ratio <- function(model, post_data, probs, return_draws) {
  obs_or <- model$data$data %>%
    dplyr::mutate(resp_id = as.integer(.data$resp_id),
                  item_id = as.integer(.data$item_id)) %>%
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") %>%
    dplyr::select(-"resp_id") %>%
    pw_or() %>%
    dplyr::rename(obs_or = "or")

  or_res <- post_data %>%
    tidyr::nest(dat = c("obs", "value", "resp", "item")) %>%
    dplyr::mutate(
      dat = lapply(.data$dat,
                   function(x) {
                     x %>%
                       dplyr::select(-"obs") %>%
                       tidyr::pivot_wider(names_from = "item",
                                          values_from = "value") %>%
                       dplyr::select(-"resp")
                   }),
      dat = lapply(.data$dat, pw_or)
    ) %>%
    tidyr::unnest("dat") %>%
    tidyr::nest(samples = -c("item_1", "item_2")) %>%
    dplyr::left_join(obs_or, by = c("item_1", "item_2")) %>%
    dplyr::mutate(ppmc_mean = vapply(.data$samples, function(.x) mean(.x$or),
                                     double(1)),
                  bounds = lapply(.data$samples,
                                  function(.x, probs) {
                                    tibble::as_tibble_row(
                                      stats::quantile(.x$or,
                                                      probs = probs,
                                                      na.rm = TRUE)
                                    )
                                  },
                                  probs = probs),
                  ppp = mapply(function(exp, obs) {
                    mean(exp$or > obs)
                  },
                  .data$samples, .data$obs_or)) %>%
    tidyr::unnest("bounds")

  if (return_draws > 0) {
    or_res <- or_res %>%
      dplyr::relocate("samples", .before = "ppp") %>%
      dplyr::mutate(
        samples = lapply(.data$samples,
                         function(x) {
                           x %>%
                             dplyr::slice_sample(prop = return_draws) %>%
                             dplyr::pull("or")
                         }))
  } else {
    or_res <- dplyr::select(or_res, -"samples")
  }

  return(or_res)
}

pw_or <- function(dat) {
  dat <- as.matrix(dat)
  p <- ncol(dat)
  ind <- t(combn(p, 2))
  nind <- nrow(ind)
  or <- numeric(nind)

  for (i in 1:nind) {
    xtab <- table(dat[, ind[i, 1]], dat[, ind[i, 2]])
    n00 <- xtab[1, 1]
    n01 <- xtab[1, 2]
    n10 <- xtab[2, 1]
    n11 <- xtab[2, 2]
    or[i] <- (n00 * n11) / (n01 * n10)
  }
  pwor <- tibble::as_tibble(ind, .name_repair = ~c("item_1", "item_2")) %>%
    dplyr::mutate(or = or)
  return(pwor)
}
