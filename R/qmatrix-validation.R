#' Add Q-matrix validation model objects
#'
#' Add Q-matrix validation metrics to fitted model objects. This function is a
#' wrapper around other functions to compute the metrics. The benefit of
#' using this wrapper is that the Q-matrix validation metrics are saved as part
#' of the model object so that time-intensive calculations do not need to be
#' repeated.
#'
#' @param mod A [measrdcm][dcm_estimate()] object.
#' @param epsilon The threshold for proportion of variance accounted for to flag
#'   items for appropriate empirical specifications. The default is .95 as
#'   implemented by de la Torre and Chiu (2016).
#' @param overwrite Logical. Indicates whether specified elements that have
#'   already been added to the estimated model should be overwritten. Default is
#'   `FALSE`.
#' @param save Logical. Only relevant if a file was specified in the
#'   [measrdcm][dcm_estimate()] object passed to `x`. If `TRUE` (the default),
#'   the model is re-saved to the specified file when new criteria are added to
#'   the `R` object. If `FALSE`, the new criteria will be added to the `R`
#'   object, but the saved file will not be updated.
#'
#' @return A modified [measrdcm][dcm_estimate()] object with the estimated
#' item-level discrimination index are added to the `$qmatrix_validation`
#' element of the fitted model.
#'
#' @references de la Torre, J., & Chiu, C.-Y. (2016). A general method of
#'   empirical Q-matrix validation. *Psychometrika, 81*(2), 253-273.
#'   \doi{10.1007/s11336-015-9467-8}
#'
#' @name qmatrix_validation
#' @export
#' @examplesIf measr_examples()
#' mod_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
#'                         identifier = "item")
#' rstn_mdm <- dcm_estimate(mod_spec, data = dcmdata::mdm_data,
#'                          identifier = "respondent", backend = "rstan",
#'                          method = "optim")
#' rstn_mdm <- add_respondent_estimates(rstn_mdm)
#' rstn_mdm <- add_qmatrix_validation(rstn_mdm)
add_qmatrix_validation <- function(mod, epsilon = .95, overwrite = FALSE,
                                   save = TRUE) {
  if (rlang::is_empty(mod@respondent_estimates)) {
    rlang::abort("error_bad_method",
                 message = glue::glue("Respondent estimates need to be added ",
                                      "to the model with ",
                                      "`add_respondent_estimates()`."))
  }

  att_post_probs <- measr_extract(mod, "attribute_prob")
  post_probs <- measr_extract(mod, "class_prob")

  item_param <- measr_extract(mod, "item_param")
  item_param <- item_param |>
    dplyr::mutate(estimate = mean(.data$estimate)) |>
    dplyr::select(!!dplyr::sym(mod$data$item_id):"estimate")
  item_ids <- item_param |>
    dplyr::distinct(.data$item_id) %>%
    tibble::rowid_to_column("new_item_id")
  item_param <- item_param |>
    dplyr::left_join(item_ids, by = "item_id") |>
    dplyr::mutate(item_id = .data$new_item_id) |>
    dplyr::select(-"new_item_id")

  qmatrix <- mod@model_spec@qmatrix
  qmatrix <- qmatrix %>%
    dplyr::select(-!!dplyr::sym(mod@data$item_identifier))

  # posterior probabilities of each class
  strc_param <- measr_extract(mod, "strc_param")
  strc_param <- strc_param %>%
    dplyr::mutate(estimate = mean(.data$estimate)) |>
    dplyr::select("class", "estimate") |>
    dplyr::mutate(class = sub("\\[", "", class),
                  class = sub("]", "", class))

  # pi matrix
  all_params <- item_param |>
    dplyr::select(-"estimate") |>
    dplyr::rename(type = "class", coefficient = "coef")

  att_dict <- tibble::tibble(att = colnames(qmatrix)) |>
    tibble::rowid_to_column("att_name") |>
    dplyr::mutate(att_name = glue::glue("att{att_name}"))

  for (ii in att_dict$att) {
    all_params <- all_params |>
      dplyr::mutate(attributes = sub(ii,
                                     att_dict |>
                                       dplyr::filter(.data$att == ii) |>
                                       dplyr::pull(.data$att_name),
                                     .data$attributes))
  }

  all_params <- all_params |>
    dplyr::mutate(attributes = dplyr::case_when(is.na(.data$attributes) ~
                                                  "intercept",
                                                TRUE ~ .data$attributes))

  all_profiles <- create_profiles(ncol(qmatrix))

  profile_params <-
    stats::model.matrix(stats::as.formula(paste0("~ .^",
                                                 max(ncol(all_profiles),
                                                     2L))),
                        all_profiles) |>
    tibble::as_tibble() |>
    tibble::rowid_to_column(var = "profile_id") |>
    tidyr::pivot_longer(-"profile_id", names_to = "parameter",
                        values_to = "valid_for_profile") |>
    dplyr::mutate(parameter = dplyr::case_when(.data$parameter == "(Intercept)" ~
                                                 "intercept",
                                               TRUE ~ gsub(":", "__",
                                                           .data$parameter)))

  pi_mat <- tidyr::expand_grid(item_id = seq_len(nrow(qmatrix)),
                               profile_id = seq_len(nrow(all_profiles))) |>
    dplyr::left_join(dplyr::select(all_params, "item_id",
                                   "parameter" = "attributes",
                                   "param_name" = "coefficient"),
                     by = "item_id",
                     multiple = "all", relationship = "many-to-many") |>
    dplyr::left_join(profile_params, by = c("profile_id", "parameter"),
                     relationship = "many-to-one") |>
    dplyr::filter(.data$valid_for_profile == 1) |>
    dplyr::select(-"valid_for_profile") |>
    dplyr::left_join(item_param |>
                       dplyr::select("coef", "estimate"),
                     by = c("param_name" = "coef")) |>
    dplyr::group_by(.data$item_id, .data$profile_id) |>
    dplyr::summarize(log_odds = sum(.data$estimate),
                     .groups = "drop") |>
    dplyr::mutate(prob = 1 / (1 + exp(-.data$log_odds))) |>
    dplyr::select(-"log_odds")

  attributes_per_item <- qmatrix %>%
    dplyr::mutate(total_atts =
                    rowSums(dplyr::across(dplyr::where(is.numeric)))) %>%
    dplyr::select("total_atts") %>%
    tibble::rowid_to_column("item_id")

  colnames(qmatrix) <- colnames(all_profiles)

  validation_output <- tibble::tibble()

  # calculate sigma_1:K* (e.g., sigma_1:2)
  for (ii in seq_len(nrow(qmatrix))) {
    max_specification <- all_profiles[nrow(all_profiles),]
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
      keep_spec <- sigma_q / max_sigma > epsilon
      if (keep_spec) {
        possible_specifications <- dplyr::bind_rows(possible_specifications, q)
      }
    }

    # choosing profile measuring the fewest attributes
    # when there is a tie, profile chosen based on proportion of variance accounted for (PVAF)
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

  if (overwrite || rlang::is_empty(mod@qmatrix_validation)) {
    mod@qmatrix_validation <- list(q_matrix_validation_output =
                                     validation_output)
  }

  # re-save model object (if applicable)
  if (!is.null(mod@file) && save) {
    write_measrfit(x, file = x@file)
  }

  return(mod)
}
