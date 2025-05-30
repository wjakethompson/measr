#' Yen's Q statistic for diagnostic models
#'
#' Calculate the Yen's Q statistic for diagnostic models.
#'
#' @param x A [measrdcm][dcm_estimate()] object.
#' @param crit_value Numerical. A critical value threshold for evaluating the
#'   residual correlations from the Yen's Q calculations.
#' @param ... Unused.
#'
#' @rdname yens-q
#' @return A tibble with the residual correlation and flags for all item pairs.
#' @export
#'
#' @references Chen, W.-H., & Thissen, D. (1997). Local dependence indexes for
#'   item pairs using item response theory. *Journal of Educational and
#'   Behavioral Statistics, 22*(3), 265-389. \doi{10.3102/10769986022003265}
#' @references Christensen, K. B., Makransky, G., & Horton, M. (2016). Critical
#'   values for Yen's Q3: Identification of local dependence in the Rasch model
#'   using residual correlations. *Applied Psychological Measurement, 41*(3),
#'   178-194. \doi{10.1177/0146621616677520}
#'
#' @examplesIf measr_examples()
#' # example code
#' model_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
#'                           identifier = "item")
#' model <- dcm_estimate(dcm_spec = model_spec, data = dcmdata::mdm_data,
#'                       identifier = "respondent", method = "optim",
#'                       seed = 63277)
#'
#' yens_q(model)
yens_q <- S7::new_generic("yens_q", "x", function(x, ..., force = FALSE) {
  S7::S7_dispatch()
})

# methods ----------------------------------------------------------------------
S7::method(yens_q, measrdcm) <- function(x, crit_value = .2, force = FALSE) {
  if (!rlang::is_empty(x@criteria$yen_q) && !force) {
    return(x@criteria$yens_q)
  }

  if (rlang::is_empty(x@respondent_estimates)) {
    rlang::abort("error_bad_method",
                 message = glue::glue("Run `add_respondent_estimates()` ",
                                      "before `add_yens_q()`."))
  }

  possible_profs <- create_profiles(x@model_spec@qmatrix |>
                                      ncol()) |>
    tidyr::unite(col = "profile", everything(), sep = "") |>
    tibble::rowid_to_column("profile_num")

  qmatrix <- x@model_spec@qmatrix

  obs <- x@data$clean_data

  item_param <- measr_extract(x, "item_param")
  item_param <- item_param |>
    dplyr::mutate(estimate = mean(.data$estimate)) |>
    dplyr::select(!!rlang::sym(x@data$item_identifier):"estimate")
  item_ids <- item_param |>
    dplyr::distinct(!!rlang::sym(x@data$item_identifier)) |>
    tibble::rowid_to_column("new_item_id")
  item_param <- item_param |>
    dplyr::left_join(item_ids, by = x@data$item_identifier) |>
    dplyr::mutate(item_id = .data$new_item_id) |>
    dplyr::select(-"new_item_id")
  all_params <- item_param |>
    dplyr::select(-"estimate")

  all_profiles <- create_profiles(ncol(qmatrix))

  if (x@model_spec@measurement_model@model %in% c("lcdm", "crum")) {
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

    profile_params <-
      stats::model.matrix(stats::as.formula(paste0("~ .^",
                                                   max(ncol(all_profiles),
                                                       2L))),
                          all_profiles) |>
      tibble::as_tibble() |>
      tibble::rowid_to_column(var = "profile_id") |>
      tidyr::pivot_longer(-"profile_id", names_to = "parameter",
                          values_to = "valid_for_profile") |>
      dplyr::mutate(parameter = dplyr::case_when(.data$parameter ==
                                                   "(Intercept)" ~
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
                         dplyr::select(coef = "coefficient", "estimate"),
                       by = c("param_name" = "coef")) |>
      dplyr::group_by(.data$item_id, .data$profile_id) |>
      dplyr::summarize(log_odds = sum(.data$estimate),
                       .groups = "drop") |>
      dplyr::mutate(prob = 1 / (1 + exp(-.data$log_odds))) |>
      dplyr::select(-"log_odds") |>
      dplyr::rename(pi = "prob")
  } else if (x@model_spec@measurement_model@model == "dina") {
    meas_by_item <- qmatrix |>
      tibble::rowid_to_column("item_id") |>
      tidyr::pivot_longer(cols = -c("item_id"), names_to = "att",
                          values_to = "meas")

    pi_mat <- tidyr::crossing(profile_id = 1:nrow(all_profiles),
                              item_id = 1:nrow(qmatrix)) |>
      dplyr::left_join(all_profiles |>
                         tibble::rowid_to_column("profile_id"),
                       by = "profile_id") |>
      tidyr::pivot_longer(cols = -c("profile_id", "item_id"), names_to = "att",
                          values_to = "mastered") |>
      dplyr::left_join(meas_by_item, by = c("item_id", "att")) |>
      dplyr::filter(.data$meas == 1) |>
      dplyr::mutate(mastered = as.numeric(.data$mastered == .data$meas)) |>
      dplyr::select(-"meas") |>
      dplyr::group_by(.data$profile_id, .data$item_id) |>
      dplyr::summarize(mastered_all = mean(.data$mastered), .groups = 'keep') |>
      dplyr::ungroup() |>
      dplyr::mutate(mastered_all = dplyr::case_when(.data$mastered_all < 1 ~ 0,
                                                    TRUE ~ 1)) |>
      dplyr::left_join(all_params |>
                         tidyr::pivot_wider(names_from = "type",
                                            values_from = "coefficient"),
                       by = "item_id") |>
      dplyr::mutate(param = dplyr::case_when(.data$mastered_all == 0 ~ guess,
                                             .data$mastered_all == 1 ~ slip)) |>
      dplyr::select(-"guess", -"slip", -"mastered_all") |>
      dplyr::left_join(item_param |>
                         dplyr::select(-"type"),
                       by = c("item_id", "param" = "coefficient")) |>
      dplyr::rename(pi = "estimate") |>
      dplyr::select(-"param")
  } else if (x@model_spec@measurement_model@model == "dino") {
    meas_by_item <- qmatrix |>
      tibble::rowid_to_column("item_id") |>
      tidyr::pivot_longer(cols = -c("item_id"), names_to = "att",
                          values_to = "meas")

    pi_mat <- tidyr::crossing(profile_id = 1:nrow(all_profiles),
                              item_id = 1:nrow(qmatrix)) |>
      dplyr::left_join(all_profiles |>
                         tibble::rowid_to_column("profile_id"),
                       by = "profile_id") |>
      tidyr::pivot_longer(cols = -c("profile_id", "item_id"), names_to = "att",
                          values_to = "mastered") |>
      dplyr::left_join(meas_by_item, by = c("item_id", "att")) |>
      dplyr::filter(.data$meas == 1) |>
      dplyr::mutate(mastered = as.numeric(.data$mastered == .data$meas)) |>
      dplyr::select(-"meas") |>
      dplyr::group_by(.data$profile_id, .data$item_id) |>
      dplyr::summarize(mastered_all = mean(.data$mastered), .groups = 'keep') |>
      dplyr::ungroup() |>
      dplyr::mutate(mastered_all = dplyr::case_when(.data$mastered_all > 0 ~ 1,
                                                    TRUE ~ 0)) |>
      dplyr::left_join(all_params |>
                         tidyr::pivot_wider(names_from = "type",
                                            values_from = "coefficient"),
                       by = "item_id") |>
      dplyr::mutate(param = dplyr::case_when(.data$mastered_all == 0 ~ guess,
                                             .data$mastered_all == 1 ~ slip)) |>
      dplyr::select(-"guess", -"slip", -"mastered_all") |>
      dplyr::left_join(item_param |>
                         dplyr::mutate(estimate =
                                         dplyr::case_when(.data$type == "slip" ~
                                                            1 - .data$estimate,
                                                          TRUE ~
                                                            .data$estimate)) |>
                         dplyr::select(-"type"),
                       by = c("item_id", "param" = "coefficient")) |>
      dplyr::rename(pi = "estimate") |>
      dplyr::select(-"param")
  } else if (x@model_spec@measurement_model@model == "nido") {

  } else if (x@model_spec@measurement_model@model == "nida") {

  } else if (x@model_spec@measurement_model@model == "ncrum") {

  }

  class_probs <- x@respondent_estimates$class_probabilities |>
    dplyr::mutate(class = gsub("(\\D|\\.(?=.*\\.))", "", .data$class,
                               perl = TRUE)) |>
    dplyr::left_join(possible_profs, by = c("class" = "profile")) |>
    dplyr::select(resp_id = !!rlang::sym(x@data$respondent_identifier),
                  class = "profile_num", "probability")

  exp_value <- class_probs |>
    dplyr::left_join(pi_mat, by = c("class" = "profile_id"),
                     relationship = "many-to-many") |>
    dplyr::mutate(exp = .data$probability * .data$pi) |>
    dplyr::group_by(.data$resp_id, .data$item_id) |>
    dplyr::summarize(exp = sum(.data$exp), .groups = 'keep') |>
    dplyr::ungroup() |>
    dplyr::left_join(obs |>
                       dplyr::left_join(item_ids,
                                        by = c("item_id" =
                                                 x@data$item_identifier)) |>
                       dplyr::select("resp_id", item_id = "new_item_id",
                                     "score"),
                     by = c("resp_id", "item_id")) |>
    dplyr::mutate(d = .data$score - .data$exp)

  num_items <- nrow(item_ids)
  yens_q <- matrix(nrow = num_items, ncol = num_items)

  for (ii in seq_len(num_items)) {
    for (jj in seq_len(num_items)) {
      if (ii == jj) {
        yens_q[ii, jj] <- 1
      } else {
        tmp_yens_q <- exp_value |>
          dplyr::filter(item_id == ii | item_id == jj) |>
          dplyr::select(-"exp", -"score") |>
          tidyr::pivot_wider(names_from = "item_id", values_from = "d") |>
          dplyr::select(-"resp_id") |>
          cor()

        yens_q[ii, jj] <- tmp_yens_q[1,2]
        yens_q[jj, ii] <- tmp_yens_q[2,1]
      }
    }
  }

  colnames(yens_q) <- 1:num_items

  yens_q <- yens_q |>
    tibble::as_tibble(.name_repair = "minimal")
  names(yens_q) <- item_ids |>
    dplyr::mutate(item_id = as.character(.data$new_item_id)) |>
    dplyr::pull()
  yens_q <- yens_q |>
    dplyr::mutate(item_id = as.character(item_ids$new_item_id)) |>
    tidyr::pivot_longer(cols = -c("item_id"), names_to = "item_id_2",
                        values_to = "resid_corr") |>
    dplyr::filter(.data$item_id < .data$item_id_2) |>
    dplyr::mutate(flag = abs(resid_corr) >= crit_value)

  return(yens_q)
}
