#' @importFrom dcm2 fit_m2
#' @export
dcm2::fit_m2

#' Estimate the \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} fit statistic for
#' diagnostic classification models
#'
#' For diagnostic classification models, the
#' \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} statistic is calculated as
#' described by Hansen et al. (2016) and Liu et al. (2016).
#'
#' @inheritParams dcm2::fit_m2
#'
#' @return A data frame created by [dcm2::fit_m2()].
#'
#' @references Hansen, M., Cai, L., Monroe, S., & Li, Z. (2016).
#'   Limited-information goodness-of-fit testing of diagnostic classification
#'   item response models. *British Journal of Mathematical and Statistical
#'   Psychology, 69*(3), 225-252. \doi{10.1111/bmsp.12074}
#' @references Liu, Y., Tian, W., & Xin, T. (2016). An application of
#'   \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} statistic to evaluate the fit
#'   of cognitive diagnostic models. *Journal of Educational and Behavioral
#'   Statistics, 41*(1), 3-26. \doi{10.3102/1076998615621293}
#'
#' @describeIn fit_m2 \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} for
#'   diagnostic classification models.
#' @export
#' @examplesIf measr_examples()
#' rstn_mdm_lcdm <- measr_dcm(
#'   data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
#'   resp_id = "respondent", item_id = "item", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' fit_m2(rstn_mdm_lcdm)
fit_m2.measrdcm <- function(model, ci = 0.9, ...) {
  model <- check_model(model, required_class = "measrdcm", name = "model")
  ci <- check_double(ci, lb = 0, ub = 1, inclusive = FALSE, name = "ci")

  item_order <- levels(model$data$data$item_id)
  dat <- model$data$data %>%
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") %>%
    dplyr::select(-"resp_id", !!!item_order) %>%
    as.matrix() %>%
    unname()

  q <- model$data$qmatrix %>%
    dplyr::select(-"item_id")

  draws <- if (model$method == "mcmc") {
    get_mcmc_draws(model)
  } else if (model$method == "optim") {
    get_optim_draws(model)
  }

  strc <- posterior::subset_draws(draws, variable = "log_Vc") %>%
    posterior::as_draws_df() %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), mean)) %>%
    dplyr::select(-c(".chain", ".iteration", ".draw")) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::mutate(value = exp(.data$value),
                  value = .data$value / sum(.data$value)) %>%
    dplyr::pull("value")

  pi <- posterior::subset_draws(draws, variable = "pi") %>%
    posterior::as_draws_df() %>%
    tibble::as_tibble() %>%
    dplyr::select(-c(".chain", ".iteration", ".draw")) %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), mean)) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    tidyr::separate_wider_regex(cols = "name",
                                patterns = c("pi\\[", item = "[0-9]*",
                                             ",", class = "[0-9]*", "\\]")) %>%
    tidyr::pivot_wider(names_from = "class", values_from = "value") %>%
    dplyr::select(-"item") %>%
    as.matrix() %>%
    unname()

  m2 <- dcm2::calc_m2(data = dat, struc_params = strc, pi_matrix = pi,
                      qmatrix = q, ci = ci, link = "logit",
                      model_type = toupper(model$type)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("ci"), ~round(.x, 4)),
                  !!glue::glue("{ci * 100}% CI") := #nolint
                    paste0("[", .data$ci_lower, ", ", .data$ci_upper, "]"),
                  .before = "srmsr")

  return(m2)
}
