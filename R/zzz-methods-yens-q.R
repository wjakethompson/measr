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
#' The Yen's Q statistic is calculated as described by Christensen et al.
#' (2016). The critical value for evaluating the residual correlations uses a
#' default value of .2 as described by Chen and Thissen (1997).
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
    x <- add_respondent_estimates(x)
  }

  possible_profs <- create_profiles(x@model_spec) |>
    tidyr::unite(col = "profile", dplyr::everything(), sep = "") |>
    tibble::rowid_to_column("profile_num")

  qmatrix <- x@model_spec@qmatrix

  obs <- x@data$clean_data

  pi_mat <- get_draws(x, vars = c("pi")) |>
    posterior::subset_draws(variable = "pi") |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "parameter", values_to = "pi") |>
    dplyr::filter(!(.data$parameter %in% c(".chain", ".iteration", ".draw"))) |>
    dplyr::mutate(parameter = sub("pi\\[", "", .data$parameter),
                  parameter = sub("\\]", "", .data$parameter)) |>
    tidyr::separate_wider_delim(col = "parameter", delim = ",",
                                names = c("item_id", "profile_id")) |>
    dplyr::mutate(profile_id = as.numeric(.data$profile_id),
                  item_id = as.numeric(.data$item_id)) |>
    dplyr::select("profile_id", "item_id", "pi")

  item_ids <- obs |>
    dplyr::distinct(.data$item_id) |>
    tibble::rowid_to_column("new_item_id")

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
    dplyr::summarize(exp = sum(.data$exp), .groups = "keep") |>
    dplyr::ungroup() |>
    dplyr::left_join(obs |>
                       dplyr::left_join(item_ids,
                                        by = c("item_id")) |>
                       dplyr::select("resp_id", item_id = "new_item_id",
                                     "score"),
                     by = c("resp_id", "item_id")) |>
    dplyr::mutate(d = .data$score - .data$exp)

  num_items <- nrow(qmatrix)
  yens_q <- matrix(nrow = num_items, ncol = num_items)

  for (ii in seq_len(num_items)) {
    for (jj in seq_len(num_items)) {
      if (ii == jj) {
        yens_q[ii, jj] <- 1
      } else {
        tmp_yens_q <- exp_value |>
          dplyr::filter(.data$item_id == ii | .data$item_id == jj) |>
          dplyr::select(-"exp", -"score") |>
          tidyr::pivot_wider(names_from = "item_id", values_from = "d") |>
          dplyr::select(-"resp_id") |>
          cor()

        yens_q[ii, jj] <- tmp_yens_q[1, 2]
        yens_q[jj, ii] <- tmp_yens_q[2, 1]
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
    dplyr::mutate(flag = abs(.data$resid_corr) >= crit_value) |>
    dplyr::left_join(item_ids |>
                       dplyr::mutate(new_item_id =
                                       as.character(.data$new_item_id)) |>
                       dplyr::rename(item = .data$item_id),
                     by = c("item_id" = "new_item_id")) |>
    dplyr::select(item_id = "item", "item_id_2", "resid_corr", "flag") |>
    dplyr::left_join(item_ids |>
                       dplyr::mutate(new_item_id =
                                       as.character(.data$new_item_id)) |>
                       dplyr::rename(item = item_id),
                     by = c("item_id_2" = "new_item_id")) |>
    dplyr::select("item_id", item_id_2 = "item", "resid_corr", "flag")

  return(yens_q)
}
