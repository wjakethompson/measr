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
#' @param force If the \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} has already
#'   been saved to the model object with [add_fit()], should it be recalculated.
#'   Default is `FALSE`.
#'
#' @return A data frame created by [dcm2::fit_m2()].
#' @name m2
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
#' @export
#' @examplesIf measr_examples()
#' rstn_mdm_lcdm <- dcm_estimate(
#'   dcm_specify(dcmdata::mdm_qmatrix, identifier = "item"),
#'   data = dcmdata::mdm_data, missing = NA, identifier = "respondent",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' fit_m2(rstn_mdm_lcdm)
`fit_m2.measr::measrdcm` <- function(model, ..., ci = 0.9, force = FALSE) {
  check_number_decimal(ci, min = 0, max = 1)
  check_bool(force)
  if (!rlang::is_empty(model@fit$m2) && !force) {
    return(model@fit$m2)
  }

  item_order <- names(model@data$item_names)
  dat <- model@data$clean_data |>
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") |>
    dplyr::select(-"resp_id", !!!item_order) |>
    as.matrix() |>
    unname()

  q <- model@model_spec@qmatrix

  draws <- get_draws(model, vars = c("log_Vc", "pi"))

  strc <- posterior::subset_draws(draws, variable = "log_Vc") |>
    posterior::as_draws_df() |>
    dplyr::summarize(dplyr::across(dplyr::everything(), mean)) |>
    dplyr::select(-c(".chain", ".iteration", ".draw")) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::mutate(
      value = exp(.data$value),
      value = .data$value / sum(.data$value)
    ) |>
    dplyr::pull("value")

  pi <- posterior::subset_draws(draws, variable = "pi") |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    dplyr::select(-c(".chain", ".iteration", ".draw")) |>
    dplyr::summarize(dplyr::across(dplyr::everything(), mean)) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    tidyr::separate_wider_regex(
      cols = "name",
      patterns = c("pi\\[", item = "[0-9]*", ",", class = "[0-9]*", "\\]")
    ) |>
    tidyr::pivot_wider(names_from = "class", values_from = "value") |>
    dplyr::select(-"item") |>
    as.matrix() |>
    unname()

  dcm2::calc_m2(
    data = dat,
    struc_params = strc,
    pi_matrix = pi,
    qmatrix = q,
    ci = ci,
    link = "logit",
    model_type = toupper(
      model@model_spec@measurement_model@model
    )
  ) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("ci"), ~ round(.x, 4)),
      !!glue::glue("{ci * 100}% CI") := paste0(
        "[",
        .data$ci_lower,
        ", ",
        .data$ci_upper,
        "]"
      ),
      .before = "srmsr"
    )
}
