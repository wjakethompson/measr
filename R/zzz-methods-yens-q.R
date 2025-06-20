#' Yen's \eqn{Q_3} statistic for local item dependence
#'
#' Calculate the \eqn{Q_3} statistic to evaluate the assumption of independent
#' items.
#'
#' @param x A [measrdcm][dcm_estimate()] object.
#' @param ... Unused.
#' @param crit_value The critical value threshold for flagging the
#'   residual correlation of a given item pair. The default is 0.2, as described
#'   by Chen and Thissen (1997).
#' @param summary A summary statistic to be returned. Must be one of `"q3max"`
#'   or `"q3star"` (see Details). If `NULL` (the default), no summary statistic
#'   is return, and all residual correlations are returned.
#'
#' @details
#' Psychometric models assume that items are independent of each other,
#' conditional on the latent trait.
#' The \eqn{Q_3} statistic (Yen, 1984) is used to evaluate this assumption.
#' For each observed item response, we calculate the residual between the model
#' predicted score and the observed score and then estimate correlations between
#' the residuals across items.
#' Each residual correlation is a \eqn{Q_3} statistic.
#'
#' Often, a critical values is used to flag a residual correlation above a given
#' threshold (e.g., Chen & Thissen, 1997).
#' Alternatively, we may use a summary statistic such as the maximum \eqn{Q_3}
#' statistic (\eqn{Q_{3,max}}; Christensen et al., 2017), defined as
#'
#' \deqn{Q_{3,max} = \text{max}_{i>j}\left|Q_{3,ij}\right|}
#'
#' Or the mean-adjusted maximum \eqn{Q_3} statistic (\eqn{Q_{3,*}};
#' Marais, 2013), defined as
#'
#' \deqn{
#'   \overline{Q}_3 = \begin{pmatrix} I\\\ 2\end{pmatrix}^{-1}
#'   \displaystyle\sum_{i>j}Q_{3,ij} \\
#'
#'   Q_{3,*} = Q_{3,max} - \overline{Q}_3
#' }
#'
#' @return If `summary = NULL`, a tibble with the residual correlation and
#'   flags for all item pairs. Otherwise, a numeric value representing the
#'   requested summary statistic.
#' @export
#'
#' @concept Chen
#' @concept Thissen
#'
#' @references Chen, W.-H., & Thissen, D. (1997). Local dependence indexes for
#'   item pairs using item response theory. *Journal of Educational and
#'   Behavioral Statistics, 22*(3), 265-389. \doi{10.3102/10769986022003265}
#' @references Christensen, K. B., Makransky, G., & Horton, M. (2017). Critical
#'   values for Yen's Q3: Identification of local dependence in the Rasch model
#'   using residual correlations. *Applied Psychological Measurement, 41*(3),
#'   178-194. \doi{10.1177/0146621616677520}
#' @references Marais, I. (2013). Local dependence. In K. B. Christensen, S.
#'   Kreiner, & M. Mesbah (Eds.), *Rasch models in health* (pp. 111-130). Wiley.
#' @references Yen, W. M. (1984). Effects of local item dependence on the fit
#'   and equating performance of the three-parameter logistic model.
#'   *Applied Psychological Measurement, 8*(2), 125-145.
#'   \doi{10.1177/014662168400800201}
#'
#' @examplesIf measr_examples()
#' # example code
#' model_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
#'                           identifier = "item")
#' model <- dcm_estimate(dcm_spec = model_spec, data = dcmdata::mdm_data,
#'                       identifier = "respondent", method = "optim",
#'                       seed = 63277)
#'
#' yens_q3(model)
yens_q3 <- S7::new_generic("yens_q3", "x", function(x, ..., crit_value = .2,
                                                    summary = NULL) {
  check_number_decimal(crit_value, min = -1, max = 1)
  if (!is.null(summary)) {
    rlang::arg_match(summary, values = c("q3max", "q3star"))
  }
  S7::S7_dispatch()
})

# methods ----------------------------------------------------------------------
S7::method(yens_q3, measrdcm) <- function(x, crit_value = .2, summary = NULL) {
  if (rlang::is_empty(x@respondent_estimates)) {
    x <- add_respondent_estimates(x)
  }

  # meta data ------------------------------------------------------------------
  possible_profs <- profile_labels(x@model_spec) |>
    dplyr::select("class", "class_id") |>
    tibble::deframe()

  pi_mat <- measr_extract(x, "pi_matrix") |>
    tidyr::pivot_longer(cols = -x@data$item_identifier,
                        names_to = "profile", values_to = "pi") |>
    dplyr::select("profile", "item", "pi")

  class_probs <- x@respondent_estimates$class_probabilities |>
    dplyr::select(resp_id = !!rlang::sym(x@data$respondent_identifier),
                  "class", "probability")

  # calculate Q3 ---------------------------------------------------------------
  obs <- x@data$clean_data
  resid_cor <- tidyr::expand_grid(resp_id = names(x@data$respondent_names),
                                  item_id = names(x@data$item_names),
                                  profile = names(possible_profs)) |>
    dplyr::semi_join(obs, by = c("resp_id", "item_id")) |>
    dplyr::left_join(class_probs, by = c("resp_id", "profile" = "class"),
                     relationship = "many-to-one") |>
    dplyr::left_join(pi_mat, by = c("item_id" = "item", "profile"),
                     relationship = "many-to-one") |>
    dplyr::summarize(
      exp = exp(log(sum(exp(log(.data$probability) + log(.data$pi)))) -
                  log(sum(.data$probability))),
      .by = c("resp_id", "item_id")
    ) |>
    dplyr::left_join(obs, by = c("resp_id", "item_id"),
                     relationship = "one-to-one") |>
    dplyr::mutate(residual = .data$score - .data$exp) |>
    dplyr::select("resp_id", "item_id", "residual") |>
    tidyr::pivot_wider(names_from = "item_id", values_from = "residual") |>
    dplyr::select(-"resp_id") |>
    stats::cor(use = "pairwise.complete.obs")

  all_cor <- tidyr::crossing(item1 = x@data$item_names,
                             item2 = x@data$item_names) |>
    dplyr::filter(.data$item1 < item2) |>
    dplyr::mutate(item1 = names(x@data$item_names)[.data$item1],
                  item2 = names(x@data$item_names)[.data$item2],
                  resid_cor = mapply(\(x, y) resid_cor[y, x],
                                     .data$item1, .data$item2),
                  resid_cor = unname(.data$resid_cor),
                  flag = abs(.data$resid_cor) > crit_value)

  # calculate summary statistic and/or return ----------------------------------
  if (is.null(summary)) return(all_cor)

  sum_stat <- if (summary == "q3max") {
    all_cor |>
      dplyr::mutate(abs_val = abs(resid_cor)) |>
      dplyr::slice_max(abs_val) |>
      dplyr::pull("abs_val")
  } else if (summary == "q3star") {
    all_cor |>
      dplyr::mutate(abs_val = abs(resid_cor)) |>
      dplyr::summarize(q3star = max(abs_val) - mean(abs_val)) |>
      dplyr::pull("q3star")
  }

  sum_stat
}
