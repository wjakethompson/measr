#' Item difficulty indices
#'
#' The item difficulty index is a measure of the difficulty of an item, often
#' represented as a proportion correct index or p-value (de la Torre & Douglas,
#' 2008). Diagnostic classification models define p-values conditioned on latent
#' class, which provide class-level information (Thompson, 2019). The item-level
#' weighted p-value is a weighted average of class-level p-values, and defines
#' the likelihood that a randomly selected respondent correctly answers an item.
#'
#' @param model The estimated model to be evaluated.
#'
#' @details
#' The item-level weighted p-value discrimination is calculated as the
#' average of class-level p-values (i.e. \eqn{\pi_{ic}}), weighted by the
#' relative frequency of each latent class (i.e. \eqn{\nu_c}).
#'
#' @return A list with one element:
#'   * `weighted_pvalue`: A [tibble][tibble::tibble-package] with one row
#'     per item containing the weighted p-value for the item.
#' @export
#'
#' @references de la Torre, J., & Douglas, J. A. (2008). Model evaluation and
#'   multiple strategies in cognitive diagnosis: An analysis of fraction
#'   subtraction data. *Psychometrika, 73*, 595–624.
#'   \doi{10.1007/s11336-008-9063-2}
#' @references Thompson, W. J. (2019). *Bayesian psychometrics for diagnostic
#'   assessments: A proof of concept* (Research Report No. 19-01). University
#'   of Kansas; Accessible Teaching, Learning, and Assessment Systems.
#'   \doi{10.35542/osf.io/jzqs8}
#' @examplesIf measr_examples()
#' rstn_ecpe_lcdm <- measr_dcm(
#'   data = ecpe_data, missing = NA, qmatrix = ecpe_qmatrix,
#'   resp_id = "resp_id", item_id = "item_id", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' difficulty(rstn_ecpe_lcdm)
difficulty <- function(model) {
  model <- check_model(model, required_class = "measrfit", name = "model")

  stan_draws <- switch(model$method,
                       "mcmc" = get_mcmc_draws(model),
                       "optim" = get_optim_draws(model))

  pi_matrix <- stan_draws %>%
    posterior::subset_draws(variable = "pi") %>%
    posterior::as_draws_df() %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) %>%
    dplyr::summarize(value = mean(.data$value), .by = "name") %>%
    tidyr::separate_wider_regex(
      cols = "name",
      patterns = c("pi\\[", item = "[0-9]*", ",", class = "[0-9]*", "\\]")
    ) %>%
    dplyr::mutate(item = as.integer(.data$item),
                  class = as.integer(.data$class))

  vc <- stan_draws %>%
    posterior::subset_draws(variable = "log_Vc") %>%
    posterior::as_draws_df() %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) %>%
    dplyr::summarize(value = mean(.data$value), .by = "name") %>%
    dplyr::mutate(value = exp(.data$value)) %>%
    tidyr::separate_wider_regex(
      cols = "name",
      patterns = c("log_Vc\\[", class = "[0-9]*", "\\]")
    ) %>%
    dplyr::mutate(class = as.integer(.data$class))


  weighted_pval <- tidyr::crossing(item = unique(pi_matrix$item),
                                   profile = unique(pi_matrix$class)) %>%
    dplyr::left_join(pi_matrix, by = c("item", "profile" = "class"),
                     relationship = "one-to-one") %>%
    dplyr::rename("pi_prob" = "value") %>%
    dplyr::left_join(vc, by = c("profile" = "class"),
                     relationship = "many-to-one") %>%
    dplyr::rename("vc_prob" = "value") %>%
    dplyr::summarize(weighted_pval = stats::weighted.mean(.data$pi_prob,
                                                          w = .data$vc_prob),
                     .by = "item")

  return(
    list(weighted_pvalue = weighted_pval)
  )
}

