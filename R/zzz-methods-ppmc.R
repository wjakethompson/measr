#' Posterior predictive model checks for assessing model fit
#'
#' For models estimated with `method = "mcmc"`, use the posterior distributions
#' to compute expected distributions for fit statistics and compare to values
#' in the observed data.
#'
#' @param x An estimated model object (e.g., from [dcm_estimate()]).
#' @param model_fit The posterior predictive model checks to compute for an
#'   evaluation of model-level fit. If `NULL`, no model-level checks are
#'   computed. See details.
#' @param item_fit The posterior predictive model checks to compute for an
#'   evaluation of item-level fit. If `NULL`, no item-level checks are computed.
#'   See details.
#' @param ndraws The number of posterior draws to base the checks on. Must be
#'   less than or equal to the total number of posterior draws retained in the
#'   estimated model. If `NULL` (the default) the total number from the
#'   estimated model is used.
#' @param probs The percentiles to be computed by the [stats::quantile()]
#'   function for summarizing the posterior distribution for each fit statistic.
#' @param return_draws Number of posterior draws for each specified fit
#'   statistic to be returned. This does not affect the calculation of the
#'   posterior predictive checks, but can be useful for visualizing the fit
#'   statistics. Must be less than `ndraws` (or the total number of draws if
#'   `ndraws = NULL`). If `0` (the default), only summaries of the posterior are
#'   returned (no individual samples).
#' @param force If all requested \acronym{PPMC}s have already been added to the
#'   model object using [add_fit()], should they be recalculated. Default is
#'   `FALSE`.
#' @param ... Unused. For future extensions.
#'
#' @details
#' Posterior predictive model checks (PPMCs) use the posterior distribution of
#' an estimated model to compute different statistics. This creates an expected
#' distribution of the given statistic, *if our estimated parameters are
#' correct*. We then compute the statistic in our observed data and compare the
#' observed value to the expected distribution. Observed values that fall
#' outside of the expected distributions indicate incompatibility between the
#' estimated model and the observed data.
#'
#' @return A list with two elements, "model_fit" and "item_fit". If either
#'   `model_fit = NULL` or `item_fit = NULL` in the function call, this will be
#'   a one-element list, with the null criteria excluded. Each list element, is
#'   itself a list with one element for each specified \acronym{PPMC} containing
#'   a [tibble][tibble::tibble-package]. For example if
#'   `item_fit = c("conditional_prob", "odds_ratio")`, the "item_fit" element
#'   will be a list of length two, where each element is a tibble containing the
#'   results of the \acronym{PPMC}. All tibbles follow the same general
#'   structure:
#'
#'   \itemize{
#'   \item `obs_{ppmc}`: The value of the relevant statistic in the observed
#'     data.
#'   \item `ppmc_mean`: The mean of the `ndraws` posterior samples calculated
#'     for the given statistic.
#'   \item Quantile columns: 1 column for each value of `probs`, providing the
#'     corresponding quantiles of the `ndraws` posterior samples calculated for
#'     the given statistic.
#'   \item `samples`: A list column, where each element contains a vector of
#'     length `return_draws`, representing samples from the posterior
#'     distribution of the calculated statistic. This column is excluded if
#'     `return_draws = 0`.
#'   \item `ppp`: The posterior predictive p-value. This is the proportion of
#'     posterior samples for calculated statistic that are greater than the
#'     observed value. Values very close to 0 or 1 indicate incompatibility
#'     between the fitted model and the observed data.
#'   }
#'
#' @references Park, J. Y., Johnson, M. S., Lee, Y-S. (2015). Posterior
#'   predictive model checks for cognitive diagnostic models. *International
#'   Journal of Quantitative Research in Education, 2*(3-4), 244-264.
#'   \doi{10.1504/IJQRE.2015.071738}
#' @references Sinharay, S., & Almond, R. G. (2007). Assessing fit of cognitive
#'   diagnostic models. *Educational and Psychological Measurement, 67*(2),
#'   239-257. \doi{10.1177/0013164406292025}
#' @references Sinharay, S., Johnson, M. S., & Stern, H. S. (2006). Posterior
#'   predictive assessment of item response theory models. *Applied
#'   Psychological Measurement, 30*(4), 298-321.
#'   \doi{10.1177/0146621605285517}
#' @references Thompson, W. J. (2019). *Bayesian psychometrics for diagnostic
#'   assessments: A proof of concept* (Research Report No. 19-01). University
#'   of Kansas; Accessible Teaching, Learning, and Assessment Systems.
#'   \doi{10.35542/osf.io/jzqs8}
#'
#' @export
#' @examplesIf measr_examples()
#' mdm_dina <- dcm_estimate(
#'   dcm_specify(dcmdata::mdm_qmatrix, identifier = "item",
#'               measurement_model = dina()),
#'   data = dcmdata::mdm_data, missing = NA, identifier = "respondent",
#'   method = "mcmc", seed = 63277, backend = "rstan",
#'   iter = 700, warmup = 500, chains = 2, refresh = 0
#' )
#'
#' fit_ppmc(mdm_dina, model_fit = "raw_score")
fit_ppmc <- S7::new_generic(
  "fit_ppmc", "x",
  function(x, ..., model_fit = NULL, item_fit = NULL,
           ndraws = NULL, probs = c(0.025, 0.975), return_draws = 0,
           force = FALSE) {
    total_draws <- posterior::ndraws(posterior::as_draws(x))
    check_number_whole(ndraws, min = 1, max = as.numeric(total_draws),
                       allow_null = TRUE)
    for (i in seq_along(probs)) {
      check_number_decimal(probs[i], min = 0, max = 1, arg = "probs")
    }
    check_number_whole(return_draws,
                       min = 0, max = as.numeric(min(ndraws, total_draws)))
    check_bool(force)

    S7::S7_dispatch()
  }
)

# measrdcm ---------------------------------------------------------------------
dcm_model_ppmc <- c("raw_score")
dcm_item_ppmc <- c("conditional_prob", "odds_ratio", "pvalue")

#' @details
#' For DCMs, we currently support PPMCs at the model and item level. At the
#' model level, we calculate the expected raw score distribution
#' (`model_fit = "raw_score"`) as described by Thompson (2019) and Park et al.
#' (2015). At the item level, we can calculate the conditional probability that
#' a respondent in each class provides a correct response (`item_fit =
#' "conditional_prob"`) as described by Thompson (2019) and Sinharay & Almond
#' (2007) or the overall proportion correct for an item (`item_fit = "pvalue"`),
#' as described by Thompson (2019). We can also calculate the odds ratio for
#' each pair of items (`item_fit = "odds_ratio"`) as described by Park et al.
#' (2015) and Sinharay et al. (2006).
#' @name fit_ppmc
S7::method(fit_ppmc, measrdcm) <- function(x,
                                           model_fit = NULL,
                                           item_fit = NULL,
                                           ndraws = NULL,
                                           probs = c(0.025, 0.975),
                                           return_draws = 0,
                                           force = FALSE) {
  if (!is.null(model_fit)) {
    model_fit <- rlang::arg_match(model_fit, values = dcm_model_ppmc,
                                  multiple = TRUE)
  }
  if (!is.null(item_fit)) {
    item_fit <- rlang::arg_match(item_fit, values = dcm_item_ppmc,
                                 multiple = TRUE)
  }

  all_ppmc <- c(model_fit, item_fit)

  # create output object -------------------------------------------------------
  if (!length(all_ppmc)) {
    return(list())
  } else if (!force) {
    all_ppmc <- paste0("ppmc_", all_ppmc)
    results <- lapply(all_ppmc, \(pc) x@fit[[pc]]) |>
      rlang::set_names(all_ppmc)

    if (!any(vapply(results, rlang::is_null, logical(1)))) return(results)
  } else {
    all_ppmc <- paste0("ppmc_", all_ppmc)
    results <- rlang::set_names(
      vector(mode = "list", length = length(all_ppmc)),
      nm = all_ppmc
    )
  }

  # generate replicated data sets ----------------------------------------------
  need_probs <- "ppmc_conditional_prob" %in% all_ppmc &&
    is.null(results$ppmc_conditional_prob)
  total_draws <- posterior::ndraws(posterior::as_draws(x))
  keep_draws <- if (is.null(ndraws)) {
    ndraws
  } else {
    sample(seq_len(total_draws), size = ndraws)
  }

  stan_data <- stan_data(x)
  stan_draws <- get_draws(x, vars = c("log_Vc", "pi")) |>
    posterior::merge_chains() |>
    posterior::subset_draws(draw = keep_draws)
  stan_args <- default_stan_args(x@backend, gqs(), draws = stan_draws)
  stan_args$data <- stan_data
  precomp <- if (need_probs) stanmodels$gqs_ppmc_probs else stanmodels$gqs_ppmc

  stan_function_call <- stan_call(
    backend = x@backend,
    method = gqs(),
    code = dcmstan::stan_code(
      dcmstan::generated_quantities(ppmc = TRUE, probabilities = need_probs)
    ),
    args = stan_args,
    precompiled = precomp
  )
  out <- capture.output( # nolint
    mod <- do.call(stan_function_call$call_function,
                   stan_function_call$args)
  )

  post_data <- extract_stan_draws(backend = x@backend, method = gqs(),
                                  model = mod, vars = "y_rep") |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) |>
    tidyr::separate_wider_regex(
      cols = "name",
      patterns = c("y_rep\\[", obs = "\\d+", "\\]")) |>
    dplyr::mutate(obs = as.integer(.data$obs)) |>
    dplyr::left_join(x@data$clean_data |>
                       dplyr::mutate(obs = seq_len(dplyr::n()),
                                     resp = as.integer(.data$resp_id),
                                     item = as.integer(.data$item_id)) |>
                       dplyr::select("obs", "resp", "item"),
                     by = "obs", relationship = "many-to-one")

  # calculate results ----------------------------------------------------------
  extra_args <- if (need_probs) {
    list(resp_prob = extract_class_probs(x, gq = mod),
         pi_draws = posterior::subset_draws(stan_draws, variable = "pi"))
  } else {
    list(resp_prob = NULL, pi_draws = NULL)
  }

  mapply(
    \(res, nm, spec, obs, post, probs, return_draws, force, resp_prob,
      pi_draws) {
      if (!is.null(res) && !force) return(res)

      do.call(nm,
              list(spec = spec, obs = obs, post = post, probs = probs,
                   return_draws, resp_prob = resp_prob, pi_draws = pi_draws))
    },
    results, names(results),
    MoreArgs = c(list(spec = x@model_spec, obs = x@data$clean_data,
                      post = post_data, probs = probs,
                      return_draws = return_draws, force = force),
                 extra_args),
    SIMPLIFY = FALSE
  ) |>
    rlang::set_names(names(results))
}


# ppmc stats -------------------------------------------------------------------
ppmc_raw_score <- function(spec, obs, post, probs, return_draws, ...) {
  raw_score_post <- dtplyr::lazy_dt(post) |>
    dplyr::summarize(raw_score = sum(.data$value), .by = c("resp", ".draw")) |>
    dplyr::count(.data$.draw, .data$raw_score) |>
    tibble::as_tibble() |>
    tidyr::complete(.data$.draw, raw_score = 0:nrow(spec@qmatrix),
                    fill = list(n = 0L))

  exp_raw_scores <- raw_score_post |>
    dplyr::summarize(exp_resp = mean(.data$n), .by = "raw_score") |>
    dplyr::mutate(exp_resp = sapply(.data$exp_resp,
                                    function(.x) max(.x, 0.0001)))

  chisq_ppmc <- raw_score_post |>
    dplyr::left_join(exp_raw_scores, by = c("raw_score"),
                     relationship = "many-to-one") |>
    dplyr::mutate(piece = ((.data$n - .data$exp_resp) ^ 2) / .data$exp_resp) |>
    dplyr::summarize(chisq = sum(.data$piece), .by = ".draw")

  chisq_obs <- obs |>
    dplyr::summarize(raw_score = sum(.data$score), .by = "resp_id") |>
    dplyr::count(.data$raw_score) |>
    tidyr::complete(raw_score = 0:nrow(spec@qmatrix),
                    fill = list(n = 0L)) |>
    dplyr::left_join(exp_raw_scores, by = "raw_score",
                     relationship = "one-to-one") |>
    dplyr::mutate(piece = ((.data$n - .data$exp_resp) ^ 2) / .data$exp_resp) |>
    dplyr::summarize(chisq = sum(.data$piece)) |>
    dplyr::pull("chisq")

  raw_score_res <- tibble::tibble(
    obs_chisq = chisq_obs,
    ppmc_mean = mean(chisq_ppmc$chisq),
    bounds = list(tibble::as_tibble_row(
      stats::quantile(chisq_ppmc$chisq, probs = probs, na.rm = TRUE)
    )),
    ppp = mean(chisq_ppmc$chisq > chisq_obs)
  ) |>
    tidyr::unnest("bounds")

  if (return_draws > 0) {
    raw_score_res <- raw_score_res |>
      dplyr::mutate(
        rawscore_samples = list(raw_score_post |>
                                  tidyr::nest(raw_scores = -".draw") |>
                                  dplyr::slice_sample(n = return_draws) |>
                                  dplyr::select(-".draw")),
        chisq_samples = list(chisq_ppmc |>
                               dplyr::slice_sample(n = return_draws) |>
                               dplyr::pull("chisq")),
        .before = "ppp")
  }

  raw_score_res
}

ppmc_conditional_prob <- function(spec, obs, resp_prob, pi_draws, probs,
                                  return_draws, ...) {
  all_profiles <- profile_labels(spec)

  obs_class <- resp_prob |>
    dplyr::mutate(dplyr::across(dplyr::where(posterior::is_rvar),
                                ~lapply(.x,
                                        function(x) {
                                          posterior::as_draws_df(x) |>
                                            tibble::as_tibble()
                                        })
    )) |>
    tidyr::unnest(-"resp_id", names_sep = "_") |>
    dplyr::select("resp_id",
                  dplyr::all_of(paste0(all_profiles$class[1], "_",
                                       c(".chain", ".iteration", ".draw"))),
                  dplyr::ends_with("_x")) |>
    dplyr::rename_with(function(x) {
      x <- sub("_x", "", x)
      x <- sub("\\[[0-9,]*\\]_", "", x)
    }) |>
    tidyr::pivot_longer(cols = -c("resp_id", ".chain", ".iteration", ".draw"),
                        names_to = "class_label",
                        values_to = "prob") |>
    dtplyr::lazy_dt() |>
    dplyr::mutate(max_class = .data$prob == max(.data$prob),
                  .by = c(".draw", "resp_id")) |>
    dplyr::filter(.data$max_class) |>
    dplyr::group_by(.data$.draw, .data$resp_id) |>
    dplyr::slice_sample(n = 1) |>
    dplyr::ungroup() |>
    dplyr::left_join(all_profiles, by = c("class_label" = "class"),
                     relationship = "many-to-one") |>
    dplyr::select(".draw", "resp_id", class = "class_id") |>
    tibble::as_tibble()

  obs_cond_pval <- obs |>
    dplyr::mutate(resp_id = as.integer(.data$resp_id),
                  item_id = as.integer(.data$item_id)) |>
    dplyr::left_join(obs_class, by = "resp_id",
                     relationship = "many-to-many") |>
    dplyr::summarize(obs_cond_pval = mean(.data$score),
                     .by = c("item_id", "class")) |>
    dplyr::arrange(.data$item_id, .data$class)

  cond_pval_res <- pi_draws |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(-c(".chain", ".iteration", ".draw"),
                        names_to = c("i", "c"), values_to = "pi",
                        names_pattern = "pi\\[([0-9]*),([0-9]*)\\]",
                        names_transform = list(
                          i = ~as.integer(.x),
                          c = ~as.integer(.x)
                        )) |>
    dplyr::select(-c(".chain", ".iteration", ".draw")) |>
    tidyr::nest(cond_pval = "pi") |>
    dplyr::arrange(.data$i, .data$c) |>
    dplyr::left_join(obs_cond_pval, by = c("i" = "item_id",
                                           "c" = "class"),
                     relationship = "one-to-one") |>
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
                  .data$cond_pval, .data$obs_cond_pval)) |>
    tidyr::unnest("bounds")

  if (return_draws > 0) {
    cond_pval_res <- cond_pval_res |>
      dplyr::mutate(
        samples = lapply(.data$cond_pval,
                         function(.x) {
                           .x |>
                             dplyr::slice_sample(n = return_draws) |>
                             dplyr::pull("pi")
                         }),
        .before = "ppp")
  }

  cond_pval_res |>
    dplyr::select(-"cond_pval") |>
    dplyr::rename(item_id = "i", class_id = "c") |>
    dplyr::left_join(all_profiles, by = "class_id",
                     relationship = "many-to-one") |>
    dplyr::mutate(item = names(spec@qmatrix_meta$item_names)[.data$item_id]) |>
    dplyr::select(-"item_id", -"class_id") |>
    dplyr::relocate("item", "class", .before = 1) |>
    dplyr::rename(!!spec@qmatrix_meta$item_identifier := "item")
}

ppmc_odds_ratio <- function(spec, obs, post, probs, return_draws, ...) {
  obs_or <- obs |>
    dplyr::mutate(resp_id = as.integer(.data$resp_id),
                  item_id = as.integer(.data$item_id)) |>
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") |>
    dplyr::select(-"resp_id") |>
    pw_or() |>
    dplyr::rename(obs_or = "or")

  or_res <- post |>
    dplyr::select(-"obs") |>
    tidyr::pivot_wider(names_from = "item",
                       values_from = "value") |>
    dplyr::select(-"resp") |>
    tidyr::nest(dat = !dplyr::starts_with(".")) |>
    dplyr::mutate(dat = lapply(.data$dat, pw_or)) |>
    tidyr::unnest("dat") |>
    tidyr::nest(samples = -c("item_1", "item_2")) |>
    dplyr::left_join(obs_or, by = c("item_1", "item_2"),
                     relationship = "one-to-one") |>
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
                  .data$samples, .data$obs_or)) |>
    tidyr::unnest("bounds")

  if (return_draws > 0) {
    or_res <- or_res |>
      dplyr::relocate("samples", .before = "ppp") |>
      dplyr::mutate(
        samples = lapply(.data$samples,
                         function(.x) {
                           .x |>
                             dplyr::slice_sample(n = return_draws) |>
                             dplyr::pull("or")
                         }))
  } else {
    or_res <- dplyr::select(or_res, -"samples")
  }

  or_res |>
    dplyr::mutate(item_1 = names(spec@qmatrix_meta$item_names)[.data$item_1],
                  item_2 = names(spec@qmatrix_meta$item_names)[.data$item_2])
}

ppmc_pvalue <- function(spec, obs, post, probs, return_draws, ...) {
  obs_pvalue <- obs |>
    dplyr::mutate(item = as.numeric(.data$item_id)) |>
    dplyr::summarize(obs_pvalue = mean(.data$score),
                     .by = c("item", "item_id"))

  pval_res <- post |>
    dplyr::summarize(pvalue = mean(.data$value),
                     .by = c(".chain", ".iteration", ".draw", "item")) |>
    tidyr::nest(samples = -"item") |>
    dplyr::left_join(obs_pvalue, by = "item", relationship = "one-to-one") |>
    dplyr::mutate(ppmc_mean = vapply(.data$samples,
                                     function(.x) mean(.x$pvalue),
                                     double(1)),
                  bounds = lapply(.data$samples,
                                  function(.x, probs) {
                                    tibble::as_tibble_row(
                                      stats::quantile(.x$pvalue,
                                                      probs = probs,
                                                      na.rm = TRUE)
                                    )
                                  },
                                  probs = probs),
                  ppp = mapply(function(exp, obs) {
                    mean(exp$pvalue > obs)
                  },
                  .data$samples, .data$obs_pvalue)) |>
    tidyr::unnest("bounds")

  if (return_draws > 0) {
    pval_res <- pval_res |>
      dplyr::relocate("samples", .before = "ppp") |>
      dplyr::mutate(
        samples = lapply(.data$samples,
                         function(x) {
                           x |>
                             dplyr::slice_sample(n = return_draws) |>
                             dplyr::pull("pvalue")
                         }))
  } else {
    pval_res <- dplyr::select(pval_res, -"samples")
  }

  pval_res |>
    dplyr::select(-"item") |>
    dplyr::rename(!!spec@qmatrix_meta$item_identifier := "item_id")
}
