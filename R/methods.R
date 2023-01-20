#' Posterior Draws of Respondent Mastery
#'
#' Calculate posterior draws of respondent mastery. Optionally retain all
#' posterior draws or return only summaries of the distribution for each
#' respondent.
#'
#' @param object An object of class `measrdcm`. Generated from [measr_dcm()].
#' @param newdata Optional new data. If not provided, the data used to estimate
#'   the model is scored. If provided, must have columns `resp_id`, `item_id`,
#'   and `score`.
#' @param summary Should summary statistics be returned instead of the raw
#'   values? Default is `TRUE`.
#' @param probs The percentiles to be computed by the `[stats::quantile()]`
#'   function. Only used if `summary` is `TRUE`.
#' @param ... Unused.
#'
#' @return A tibble. If summary is `FALSE`, a tibble with number of rows equal
#'   to the number of draws in `model` and 5 columns: `.chain`, `.iteration`,
#'   `.draw`, `resp_id`, and `master_prob`. If summary is `TRUE`, a tibble with
#'   number of rows equal to the number of respondents, and columns of
#'   `resp_id`, `mean`, and one column for every value specified in `probs`.
#' @export
predict.measrdcm <- function(object, newdata = NULL, identifier, missing = NA,
                             summary = TRUE, probs = c(0.025, 0.975), ...)  {
  model <- check_model(object, required_class = "measrdcm", name = "object")

  if (!is.null(newdata)) {
    score_data <- check_newdata(newdata, identifier = identifier, model = model,
                                missing = missing, name = "newdata")
  } else {
    score_data <- model$data$data
  }

  clean_qmatrix <- model$data$qmatrix %>%
    dplyr::select(-"item_id") %>%
    dplyr::rename_with(~glue::glue("att{1:(ncol(model$data$qmatrix) - 1)}"))
  stan_data <- create_stan_data(dat = score_data, qmat = clean_qmatrix,
                                type = model$type)

  # compile model -----
  func_name <- rlang::sym(paste0(model$type, "_script"))
  script_call <- rlang::call2(func_name,
                              rlang::expr(clean_qmatrix),
                              prior = NULL,
                              gqs = TRUE)
  stan_code <- eval(script_call)
  comp_mod <- rstan::stan_model(model_code = stan_code$stancode)

  if (model$method == "mcmc") {

  } else if (model$method == "optim") {
    draws <- get_optim_draws(model)
    gqs_mod <- rstan::gqs(comp_mod, data = stan_data, draws = draws)
    mod_chains <- 1L
    mod_iters <- 1L
  }

  if ("bayes_dlmlca" %in% class(model)) {
    prc_model <- rstan::gqs(stan_model, data = prc_data,
                            draws = as.matrix(model$model))
    mod_chains <- length(model$model@sim$samples)
    mod_iters <- model$model@stan_args[[1]][["iter"]] -
      model$model@stan_args[[1]][["warmup"]]
  } else if (model$backend == "cmdstanr" & "optim_dlmlca" %in% class(model)) {
    prc_model <- rstan::gqs(stan_model, data = prc_data,
                            draws = as.matrix(model$model$draws()))
    mod_chains <- 1L
    mod_iters <- 1L
  } else if (model$backend == "rstan" & "optim_dlmlca" %in% class(model)) {
    prc_model <- rstan::gqs(stan_model, data = prc_data,
                            draws = t(as.matrix(model$model$par)))
    mod_chains <- 1L
    mod_iters <- 1L
  }

  mastery <- as.data.frame(prc_model) %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::matches("prob_resp_class")) %>%
    tibble::add_column(.chain = rep(1:mod_chains, each = mod_iters),
                       .iteration = rep(1:mod_iters, times = mod_chains),
                       .draw = 1:(mod_chains * mod_iters),
                       .before = 1) %>%
    tidybayes::spread_draws((!!sym("prob_resp_class"))[!!!syms(c("j",
                                                                 "c"))]) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$c == 2L) %>%
    dplyr::select(.data$.chain, .data$.iteration, .data$.draw,
                  resp_id = .data$j, master_prob = .data$prob_resp_class)

  if (!is.null(newdata)) {
    mastery <- mastery %>%
      dplyr::left_join(new_resp, by = c("resp_id" = "stan_resp")) %>%
      dplyr::mutate(resp_id = .data$orig_resp) %>%
      dplyr::select(-.data$orig_resp)
  } else {
    mastery <- mastery %>%
      dplyr::left_join(model$data_lookup$resp_lookup,
                       by = c("resp_id" = "stan_resp")) %>%
      dplyr::mutate(resp_id = .data$orig_resp) %>%
      dplyr::select(-.data$orig_resp)
  }

  if (!summary) return(mastery)

  mastery <- mastery %>%
    dplyr::group_by(.data$resp_id) %>%
    dplyr::summarize(mean = mean(.data$master_prob),
                     bounds = list(tibble::enframe(
                       stats::quantile(.data$master_prob, probs = probs)
                     ))) %>%
    tidyr::unnest(.data$bounds) %>%
    tidyr::pivot_wider(names_from = .data$name, values_from = .data$value)

  return(mastery)
}
