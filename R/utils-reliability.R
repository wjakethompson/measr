create_reli_list_optim <- function(model) {
  N <- nrow(model$data_lookup$resp_lookup)
  K <- 1L
  M  <- 2 ^ K
  A <- c(0L, 1L)
  pAhatA <- double(M * K)

  attr_probs <- predict(model) %>%
    dplyr::select(`1` = mean)
  class_probs <- attr_probs %>%
    dplyr::mutate(`0` = 1 - .data$`1`, .before = 1)

  # map estimates
  ahat <- attr_probs %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~dplyr::case_when(.x >= 0.5 ~ 1L,
                                                  TRUE ~ 0L))) %>%
    as.matrix() %>%
    unname() %>%
    as.vector()

  # eap estimates
  amean <- attr_probs %>%
    as.matrix() %>%
    unname() %>%
    as.vector()

  # class probs
  pAx <- class_probs %>%
    as.matrix() %>%
    unname() %>%
    t() %>%
    as.vector()

  # structural parameters
  if (model$backend == "cmdstanr") {
    pxi <- tidybayes::spread_draws(model$model, (!!sym("nu"))[!!sym("c")]) %>%
      dplyr::summarize(nu = mean(.data$nu)) %>%
      dplyr::pull(.data$nu)
  } else if (model$backend == "rstan") {
    pxi <- model$model$par %>%
      tibble::enframe() %>%
      dplyr::filter(stringr::str_detect(.data$name, "^nu")) %>%
      dplyr::pull(.data$value)
  }

  # make format
  res <- calc_format(N = N, K = K, M = M, A = A, pAhatA = pAhatA,
                     attr_probs = attr_probs, class_probs = class_probs,
                     ahat = ahat, amean = amean, pAx = pAx, pxi = pxi)

  # coerce to lcdmrfit
  mod_dat <- model$data_lookup$data %>%
    tidyr::pivot_wider(names_from = .data$item_id,
                       values_from = .data$score) %>%
    dplyr::select(-.data$resp_id) %>%
    as.matrix()

  num_items <- nrow(model$data_lookup$item_lookup)
  item_models <- list(~a1) %>%
    rep(times = num_items) %>%
    purrr::set_names(glue::glue("Item{seq_len(num_items)}"))

  if (model$backend == "cmdstanr") {
    item_params <- model$model %>%
      tidybayes::spread_draws((!!sym("intercept"))[!!sym("i")],
                              (!!sym("maineffect"))[!!sym("i")]) %>%
      dplyr::summarize(`(Intercept)` = mean(.data$intercept),
                       a1 = mean(.data$maineffect)) %>%
      tidyr::nest(params = -.data$i) %>%
      purrr::pmap(function(i, params) {
        params %>%
          tidyr::pivot_longer(dplyr::everything(), names_to = "param",
                              values_to = "value") %>%
          tibble::deframe()
      }) %>%
      purrr::set_names(glue::glue("Item{seq_len(num_items)}"))
  } else if (model$backend == "rstan") {
    item_params <- model$model$par %>%
      tibble::enframe() %>%
      dplyr::filter(stringr::str_detect(.data$name,
                                        "^intercept|^maineffect")) %>%
      tidyr::separate(.data$name, c("param", "i", NA), sep = "\\[|\\]") %>%
      tidyr::pivot_wider(names_from = .data$param,
                         values_from = .data$value) %>%
      dplyr::rename(`(Intercept)` = .data$intercept, a1 = .data$maineffect) %>%
      tidyr::nest(params = -.data$i) %>%
      purrr::pmap(function(i, params) {
        params %>%
          tidyr::pivot_longer(dplyr::everything(), names_to = "param",
                              values_to = "value") %>%
          tibble::deframe()
      }) %>%
      purrr::set_names(glue::glue("Item{seq_len(num_items)}"))
  }

  mod_map <- res$eap %>%
    tibble::as_tibble(.name_repair = ~"a1") %>%
    dplyr::mutate(a1 = round(.data$a1, digits = 0),
                  a1 = as.integer(.data$a1)) %>%
    as.matrix() %>%
    unname()

  if (model$backend == "cmdstanr") {
    log_lik <- model$model$lp()
  } else if (model$backend == "rstan") {
    log_lik <- model$model$par %>%
      tibble::enframe() %>%
      dplyr::filter(stringr::str_detect(.data$name, "^log_lik")) %>%
      dplyr::pull(.data$value) %>%
      sum()
  }


  lcdmr::lcdmrfit(x = mod_dat,
                  item_models = item_models,
                  beta = item_params,
                  posterior = res$posterior,
                  map = mod_map,
                  eap = res$eap,
                  log_lik = log_lik,
                  acc = res$acc,
                  consist = res$consist,
                  gamma = res$gamma,
                  prev = res$prev,
                  pxi = res$pxi)
}

create_reli_list_mcmc <- function(x) {

}
