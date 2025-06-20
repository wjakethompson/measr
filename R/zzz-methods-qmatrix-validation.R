#' Add Q-matrix validation model objects
#'
#' Add Q-matrix validation metrics to fitted model objects. This function is a
#' wrapper around other functions to compute the metrics. The benefit of
#' using this wrapper is that the Q-matrix validation metrics are saved as part
#' of the model object so that time-intensive calculations do not need to be
#' repeated.
#'
#' @param x A [measrdcm][dcm_estimate()] object.
#' @param epsilon The threshold for proportion of variance accounted for to flag
#'   items for appropriate empirical specifications. The default is .95 as
#'   implemented by de la Torre and Chiu (2016).
#' @param ... Unused.
#' @param force If the criterion has already been added to the
#'   model object with [add_criterion()], should it be recalculated. Default is
#'   `FALSE`.
#'
#' @return A tibble containing the estimated item-level discrimination indices
#' used to empirically validate the Q-matrix.
#'
#' @references de la Torre, J., & Chiu, C.-Y. (2016). A general method of
#'   empirical Q-matrix validation. *Psychometrika, 81*(2), 253-273.
#'   \doi{10.1007/s11336-015-9467-8}
#'
#' @name qmatrix_validation
#' @export
#' @examplesIf measr_examples()
#' mod_spec <- dcm_specify(qmatrix = dcmdata::ecpe_qmatrix,
#'                         measurement_model = dcmstan::lcdm(),
#'                         structural_model =
#'                           dcmstan::hdcm(hierarchy =
#'                                           paste0("lexical -> cohesive -> ",
#'                                                  "morphosyntactic")),
#'                         identifier = "item_id")
#' rstn_ecpe <- dcm_estimate(mod_spec, data = dcmdata::ecpe_data,
#'                           identifier = "resp_id", backend = "rstan",
#'                           method = "optim")
#' rstn_ecpe <- add_respondent_estimates(rstn_ecpe)
#' q_matrix_validation <- qmatrix_validation(rstn_ecpe)
qmatrix_validation <- S7::new_generic("qmatrix_validation", "x",
                                      function(x, ..., epsilon = .95,
                                               force = FALSE) {
                                        S7::S7_dispatch()
                                      })

# methods ----------------------------------------------------------------------
S7::method(qmatrix_validation, measrdcm) <- function(x, epsilon = .95,
                                                     force = FALSE) {
  if (!rlang::is_empty(x@qmatrix_validation) && !force) {
    return(x@qmatrix_validation)
  }

  if (ncol(x@model_spec@qmatrix) == 1) {
    rlang::abort("error_bad_method",
                 message = glue::glue("The Q-matrix validation method can ",
                                      "only be applied to assessments ",
                                      "measuring more than one attribute."))
  }

  if (rlang::is_empty(x@respondent_estimates)) {
    x <- add_respondent_estimates(x)
  }

  qmatrix <- x@model_spec@qmatrix
  all_profiles <- create_profiles(x@model_spec)
  names(qmatrix) <- names(all_profiles)

  pi_mat <- measr:::get_draws(x, vars = c("pi")) |>
    posterior::subset_draws(variable = "pi") |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "parameter", values_to = "pi") |>
    dplyr::filter(!(.data$parameter %in% c(".chain", ".iteration", ".draw"))) |>
    dplyr::mutate(parameter = sub("pi\\[", "", .data$parameter),
                  parameter = sub("]", "", .data$parameter)) |>
    tidyr::separate_wider_delim(cols = "parameter", delim = ",",
                                names = c("item_id", "profile_id")) |>
    dplyr::select("profile_id", "item_id", "prob" = "pi") |>
    dplyr::mutate(profile_id = as.numeric(.data$profile_id),
                  item_id = as.numeric(.data$item_id))

  # posterior probabilities of each class
  strc_param <- measr_extract(x, "strc_param")
  strc_param <- strc_param |>
    dplyr::mutate(estimate = E(.data$estimate)) |>
    dplyr::select("class", "estimate") |>
    dplyr::mutate(class = sub("\\[", "", class),
                  class = sub("]", "", class))

  validation_output <- tibble::tibble()

  # calculate sigma_1:K* (e.g., sigma_1:2)
  for (ii in seq_len(nrow(qmatrix))) {
    max_specification <- all_profiles[nrow(all_profiles), ]
    max_sigma <- calc_sigma(q = max_specification, strc_param = strc_param,
                            pi_mat = pi_mat, ii)

    max_specification <- max_specification |>
      dplyr::mutate(pvaf = max_sigma / max_sigma)

    possible_specifications <- tibble::tibble()
    possible_specifications <- dplyr::bind_rows(possible_specifications,
                                                max_specification)

    for (jj in 2:(nrow(all_profiles) - 1)) {
      q <- all_profiles[jj, ]
      sigma_q <- calc_sigma(q = q, strc_param = strc_param, pi_mat = pi_mat,
                            ii = ii)

      # calculate sigma / sigma_1:K (i.e., PVAF)
      pvaf <- sigma_q / max_sigma
      q <- q |>
        dplyr::mutate(pvaf = pvaf)

      # flagging profiles where sigma / sigma_1:K < epsilon
      # only profiles where sigma / sigma_1:K >= epsilon are appropriate
      keep_spec <- (sigma_q / max_sigma) > epsilon
      if (keep_spec) {
        possible_specifications <- dplyr::bind_rows(possible_specifications, q)
      }
    }

    # choosing profile measuring the fewest attributes
    # when there is a tie, profile chosen based on proportion of variance
    # accounted for (PVAF)
    correct_spec <- possible_specifications |>
      dplyr::select(-"pvaf") |>
      dplyr::mutate(total_atts =
                      rowSums(dplyr::across(dplyr::where(is.numeric)))) |>
      dplyr::filter(.data$total_atts == min(.data$total_atts)) |>
      dplyr::select(-"total_atts") |>
      dplyr::left_join(possible_specifications, by = colnames(all_profiles)) |>
      dplyr::filter(.data$pvaf == max(.data$pvaf))

    final_pvaf <- correct_spec |>
      dplyr::pull(.data$pvaf)

    correct_spec <- correct_spec |>
      dplyr::select(-"pvaf")

    actual_spec <- qmatrix[ii, ]
    validation_flag <- nrow(dplyr::anti_join(correct_spec, actual_spec,
                                             by = colnames(actual_spec))) != 0

    item_output <- tibble::tibble(item_id = ii,
                                  validation_flag = validation_flag,
                                  original_specification = list(actual_spec),
                                  empirical_specification = list(correct_spec),
                                  pvaf = final_pvaf)
    validation_output <- dplyr::bind_rows(validation_output, item_output)
  }

  return(validation_output)
}
