#' Log marginal likelihood calculation
#'
#' Calculate the log maringal likelihood with bridge sampling (Meng & Wong,
#' 1996). This is a wrapper around [bridgesampling::bridge_sampler()].
#' Therefore, log marginal likelihood calculation is currently only available
#' for models estimated with {rstan} using MCMC.
#'
#' @param x A [measrdcm][dcm_estimate()] object estimated with
#'   `backend = "optim"`.
#' @param ... Unused.
#' @param force If the criterion has already been added to the
#'   model object with [add_criterion()], should it be recalculated. Default is
#'   `FALSE`.
#'
#' @return The estimate of the log marginal likelihood.
#' @export
#'
#' @references Meng, X.-L., & Wong, W. H. (1996). Simulating ratios of
#'   normalizing constants via a simple identity: A theoretical exploration.
#'   *Statistical Sinica, 6*(4), 831-860.
#'   <https://www.jstor.org/stable/24306045>
#'
#' @examplesIf measr_examples()
#' model_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
#'                           identifier = "item")
#' model <- dcm_estimate(dcm_spec = model_spec, data = dcmdata::mdm_data,
#'                       identifier = "respondent", method = "mcmc",
#'                       seed = 63277)
#'
#' log_mll(model)
log_mll <- S7::new_generic(
  "log_mll",
  "x",
  function(x, ..., force = FALSE) {
    S7::S7_dispatch()
  }
)

S7::method(log_mll, measrdcm) <- function(x, force = FALSE) {
  if (!rlang::is_empty(x@criteria$log_mll) && !force) {
    return(x@criteria$log_mll)
  }

  if (!(S7::S7_inherits(x@method, mcmc) && S7::S7_inherits(x@backend, rstan))) {
    cli::cli_abort(
      glue::glue(
        "{{.arg {rlang::caller_arg(x)}}} must be a model estimated with ",
        "{{.code method = \"mcmc\"}} and {{.code backend = \"rstan\"}} to ",
        "calculate the marginal likelihood"
      )
    )
  }

  log_marg_lik <- bridgesampling::bridge_sampler(x@model, silent = TRUE)
  log_marg_lik$logml
}

#' Bayes factor for model comparisons
#'
#' Calculate the Bayes factor for model comparisons, which represents the
#' posterior odds of the null hypothesis when the prior probability of the null
#' model is 0.5 (Jeffreys, 1935; Kass & Raftery, 1995).
#' Consistent with the Bayesian reporting guidelines from Kruschke (2021), we
#' calculate the posterior probability of the null model for a variety of prior
#' probabilities, in addition to the Bayes factor.
#'
#' @param x A [measrdcm][dcm_estimate()] object.
#' @param ... Additional [measrdcm][dcm_estimate()] to be compared to `x`.
#' @param model_names Names given to each provided model in the comparison
#'   output. If `NULL` (the default), the names will be parsed from the names of
#'   the objects passed for comparison.
#' @param prior_prob A numeric vector of prior probabilities for the null model
#'   used to calculate the posterior probability of the null model relative to
#'   alternative model. See details for more information.
#'
#' @details
#' Bayes factors will be calculated for all possible pairwise comparisons
#' between the models provided to `x` and `...`. In each comparison, one model
#' is identified as the null model, and the other is the alternative. This
#' distinction is not terribly meaningful from a calculation standpoint, as the
#' probabilities for the alternative model are simply 1 minus the null
#' probabilities. If you want particular models to be labeled as the "null", the
#' determination is made by the order the models are sent to the function. That
#' is, `x` will always be the null model. The first model included in `...` will
#' be the alternative model when compared to `x` and the null model when
#' compared to all other models included in `...`. Similarly, the second model
#' included in `...` will be the alternative model when compared to `x` and the
#' first model included in `...` and the null model in all other comparisons.
#'
#' `prior_prob` is used to specify a vector of possible prior probabilities for
#' the null model. These are used in conjunction with the Bayes factor to
#' determine the posterior model probability for the null model, relative to the
#' alternative model. The posterior probability for the alternative model can
#' be calculated as 1 minus the null model's posterior probability. You may
#' specify a specific prior probability, or specify a range of possibilities to
#' construct a graph similar to Kruschke's (2021) Figure 1. These probabilities
#' can be interpreted as, "If the prior probability is \{`prior_prob_null`\},
#' then the posterior is \{`posterior_prob_null`\}" (or 1 minus for the
#' alternative model).
#'
#' @concept Bayes
#'
#' @return A [tibble][tibble::tibble-package] with one row per model comparison
#'   and four columns.
#'   * `null_model`: The null model in the comparison.
#'   * `alt_model`: The alternative model in the comparison.
#'   * `bf`: The estimated Bayes factor.
#'   * `posterior_probs`: A nested list column, where element element is a
#'     tibble with two columns:
#'     * `prior_prob_null`: The prior probability that the null model is
#'       correct.
#'     * `posterior_prob_null`: The posterior probability that the null model is
#'       correct.
#'
#'     The list column can be unnested with [tidyr::unnest()] (see examples). If
#'     `prior_prob` is `NULL`, the `posterior_probs` column is excluded from
#'     the returned object.
#' @export
#'
#' @references Jeffreys, H. (1935). Some test os significance, treated by the
#'   theory of probability. *Mathematical Proceedings of the Cambridge
#'   Philosophical Society, 31*(2), 203-222. \doi{10.1017/S030500410001330X}
#' @references Kass, R. E., & Raftery, A. E. (1995). Bayes factors.
#'   *Journal of the American Statistical Association, 90*(430), 773-795.
#'   \doi{10.1080/01621459.1995.10476572}
#' @references Kruschke, J. K. (2021). Bayesian analysis reporting guidelines.
#'   *Nature, 5*, 1282-1291. \doi{10.1038/s41562-021-01177-7}
#'
#' @examplesIf measr_examples()
#' mdm_dina <- dcm_estimate(
#'   dcm_specify(dcmdata::mdm_qmatrix, identifier = "item",
#'               measurement_model = dina()),
#'   data = dcmdata::mdm_data, missing = NA, identifier = "respondent",
#'   method = "mcmc", seed = 63277, backend = "rstan",
#'   iter = 700, warmup = 500, chains = 2, refresh = 0
#' )
#'
#' mdm_dino <- dcm_estimate(
#'   dcm_specify(dcmdata::mdm_qmatrix, identifier = "item",
#'               measurement_model = dino()),
#'   data = dcmdata::mdm_data, missing = NA, identifier = "respondent",
#'   method = "mcmc", seed = 63277, backend = "rstan",
#'   iter = 700, warmup = 500, chains = 2, refresh = 0
#' )
#'
#' bf <- bayes_factor(mdm_dina, mdm_dino)
#' bf
#'
#' tidyr::unnest(bf, "posterior_probs")
bayes_factor <- S7::new_generic(
  "bayes_factor",
  "x",
  function(x, ..., model_names = NULL, prior_prob = seq(.02, .98, by = .02)) {
    for (i in seq_along(prior_prob)) {
      check_number_decimal(prior_prob[i], min = 0, max = 1, arg = "prior_prob")
    }

    S7::S7_dispatch()
  }
)

S7::method(bayes_factor, measrdcm) <- function(
  x,
  ...,
  model_names = NULL,
  prior_prob = seq(.02, .98, by = .02)
) {
  dots <- rlang::dots_list(..., .named = TRUE)
  dots_check <- vapply(dots, S7::S7_inherits, logical(1), class = measrdcm)
  if (!all(dots_check)) {
    msg <- paste(
      "{.arg {cli::cli_vec(names(dots)[!dots_check])}} must",
      "{?be a/be a/all be} {.cls measrdcm} object{?s}"
    )
    cli::cli_abort(msg)
  }
  all_models <- c(list(x), dots)

  if (is.null(model_names)) {
    model_names <- c(rlang::caller_arg(x), names(dots))
  } else if (length(model_names) != length(all_models)) {
    rdcmchecks::abort_bad_argument(
      arg = "model_names",
      must = glue::glue(
        "be of length {length(all_models)}, ",
        "the same as the number of models provided"
      ),
      not = length(model_names)
    )
  }

  all_models <- rlang::set_names(all_models, model_names)
  bf <- combn(model_names, m = 2) |>
    t() |>
    as.data.frame() |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      mod1 = all_models[.data$V1],
      mod2 = all_models[.data$V2],
      bf = calc_bf(mod1, mod2)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(null_model = "V1", alt_model = "V2", "bf")

  if (!is.null(prior_prob)) {
    bf <- dplyr::mutate(
      bf,
      posterior_probs = mapply(
        calc_model_probabilities,
        all_models[.data$null_model],
        all_models[.data$alt_model],
        MoreArgs = list(prior_prob = prior_prob),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
    )
  }

  bf
}

calc_bf <- function(x, y) {
  log_marg_lik1 <- log_mll(x)
  log_marg_lik2 <- log_mll(y)

  bf <- exp(log_marg_lik1 - log_marg_lik2)
}

calc_model_probabilities <- function(x, y, prior_prob) {
  tibble::tibble(
    model_1 = log_mll(x),
    model_2 = log_mll(y),
    prior_prob = prior_prob
  ) |>
    dplyr::mutate(
      log_diff = (model_1 + log(.data$prior_prob)) -
        (model_2 + log(1 - .data$prior_prob)),
      posterior_prob = exp(log_diff) / (1 + exp(log_diff))
    ) |>
    dplyr::select(
      prior_prob_null = "prior_prob",
      posterior_prob_null = "posterior_prob"
    )
}
