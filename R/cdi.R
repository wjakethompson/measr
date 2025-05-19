#' Item, attribute, and test-level discrimination indices
#'
#' The cognitive diagnostic index (\acronym{CDI}) is a measure of how well an
#' assessment is able to distinguish between attribute profiles.
#' The index was originally proposed by Henson & Douglas (2005) for item- and
#' test-level discrimination, and then expanded by Henson et al. (2008) to
#' include attribute-level discrimination indices.
#'
#' @param model The estimated model to be evaluated.
#' @param weight_prevalence Logical indicating whether the discrimination
#'   indices should be weighted by the prevalence of the attribute profiles. See
#'   details for additional information.
#'
#' @details
#' Henson et al. (2008) described two attribute-level discrimination indices,
#' \eqn{\mathbf{d}_{(A)\mathbf{\cdot}}} (Equation 8) and
#' \eqn{\mathbf{d}_{(B)\mathbf{\cdot}}} (Equation 13), which are similar in that
#' both are the sum of item-level discrimination indices.
#' In both cases, item-level discrimination indices are calculated as the
#' average of Kullback-Leibler information for all pairs of attributes profiles
#' for the item.
#' The item-level indices are then summed to achieve the test-level
#' discrimination index for each attribute, or the test overall.
#' However, whereas \eqn{\mathbf{d}_{(A)\mathbf{\cdot}}} is an unweighted
#' average of the Kullback-Leibler information,
#' \eqn{\mathbf{d}_{(B)\mathbf{\cdot}}} is a weighted average, where the weight
#' is defined by the prevalence of each profile (i.e.,
#' [`measr_extract(model, what = "strc_param")`][measr_extract()]).
#'
#' @return A list with two elements:
#'   * `item_discrimination`: A [tibble][tibble::tibble-package] with one row
#'     per item containing the \acronym{CDI} for the item and any relevant
#'     attributes.
#'   * `test_discrimination`: A [tibble][tibble::tibble-package] with one row
#'     containing the total \acronym{CDI} for the assessment and for each
#'     attribute.
#' @export
#'
#' @references Henson, R., & Douglas, J. (2005). Test construction for cognitive
#'   diagnosis. *Applied Psychological Measurement, 29*(4), 262-277.
#'   \doi{10.1177/0146621604272623}
#' @references Henson, R., Roussos, L., Douglas, J., & Xuming, H. (2008).
#'   Cognitive diagnostic attribute-level discrimination indices.
#'   *Applied Psychological Measurement, 32*(4), 275-288.
#'   \doi{10.1177/0146621607302478}
#' @examplesIf measr_examples()
#' rstn_ecpe_lcdm <- dcm_estimate(
#'   dcm_specify(dcmdata::ecpe_qmatrix, identifier = "item_id"),
#'   data = dcmdata::ecpe_data, missing = NA, identifier = "resp_id",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' cdi(rstn_ecpe_lcdm)
cdi <- function(model, weight_prevalence = TRUE) {
  rdcmchecks::check_S7(model, class = "measrdcm")
  check_bool(weight_prevalence)

  stan_draws <- get_draws(model, vars = c("log_Vc", "pi"))

  pi_matrix <- posterior::subset_draws(stan_draws, variable = "pi") |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) |>
    dplyr::summarize(value = E(.data$value), .by = "name") |>
    tidyr::separate_wider_regex(
      cols = "name",
      patterns = c("pi\\[", item = "[0-9]*", ",", class = "[0-9]*", "\\]")
    ) |>
    dplyr::mutate(item = as.integer(.data$item),
                  class = as.integer(.data$class))

  hamming <- profile_hamming(
    dplyr::select(measr_extract(model, "classes"), -"class")
  )
  att_names <- hamming |>
    dplyr::select(-c("profile_1", "profile_2", "hamming")) |>
    colnames()

  item_discrim <- tidyr::crossing(item = unique(pi_matrix$item),
                                  profile_1 = unique(pi_matrix$class),
                                  profile_2 = unique(pi_matrix$class)) |>
    dplyr::left_join(pi_matrix, by = c("item", "profile_1" = "class"),
                     relationship = "many-to-one") |>
    dplyr::rename("prob_1" = "value") |>
    dplyr::left_join(pi_matrix, by = c("item", "profile_2" = "class"),
                     relationship = "many-to-one") |>
    dplyr::rename("prob_2" = "value") |>
    dplyr::mutate(kli = (.data$prob_1 * log(.data$prob_1 / .data$prob_2)) +
                    ((1 - .data$prob_1) *
                       log((1 - .data$prob_1) / (1 - .data$prob_2)))) |>
    dplyr::left_join(hamming, by = c("profile_1", "profile_2"),
                     relationship = "many-to-one") |>
    dplyr::mutate(dplyr::across(dplyr::where(is.logical),
                                \(x) {
                                  dplyr::case_when(
                                    x & .data$hamming == 1L ~ TRUE,
                                    .default = NA
                                  )
                                }),
                  dplyr::across(dplyr::where(is.logical),
                                \(x) as.integer(x) * .data$kli)) |>
    dplyr::filter(.data$hamming > 0) |>
    dplyr::mutate(weight = 1 / .data$hamming)

  if (weight_prevalence) {
    vc <- stan_draws |>
      posterior::subset_draws(variable = "log_Vc") |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) |>
      dplyr::mutate(value = exp(.data$value)) |>
      dplyr::summarize(value = E(.data$value), .by = "name") |>
      tidyr::separate_wider_regex(
        cols = "name",
        patterns = c("log_Vc\\[", class = "[0-9]*", "\\]")
      ) |>
      dplyr::mutate(class = as.integer(.data$class))

    item_discrim <- item_discrim |>
      dplyr::left_join(vc, by = c("profile_1" = "class")) |>
      dplyr::mutate(weight = .data$weight * .data$value) |>
      dplyr::select(-"value")
  }

  item_discrim <- item_discrim |>
    dplyr::summarize(
      overall = stats::weighted.mean(.data$kli, w = .data$weight),
      dplyr::across(
        dplyr::all_of(att_names),
        \(x) stats::weighted.mean(x, w = .data$weight, na.rm = TRUE)
      ),
      .by = "item"
    )

  test_discrim <- item_discrim |>
    dplyr::summarize(dplyr::across(-"item", sum))

  list(item_discrimination = item_discrim,
       test_discrimination = test_discrim)
}

profile_hamming <- function(profiles) {
  profile_combos <- tidyr::crossing(profile_1 = seq_len(nrow(profiles)),
                                    profile_2 = seq_len(nrow(profiles)))


  hamming <- mapply(hamming_distance, profile_combos$profile_1,
                    profile_combos$profile_2,
                    MoreArgs = list(profiles = profiles),
                    SIMPLIFY = FALSE, USE.NAMES = FALSE) |>
    dplyr::bind_rows()

  dplyr::bind_cols(profile_combos, hamming)
}

hamming_distance <- function(prof1, prof2, profiles) {
  pattern1 <- profiles[prof1, ]
  pattern2 <- profiles[prof2, ]

  pattern1 |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "att", values_to = "patt1") |>
    dplyr::left_join(tidyr::pivot_longer(pattern2, cols = dplyr::everything(),
                                         names_to = "att", values_to = "patt2"),
                     by = "att", relationship = "one-to-one") |>
    dplyr::mutate(mismatch = .data$patt1 != .data$patt2,
                  hamming = sum(.data$mismatch)) |>
    dplyr::select("att", "mismatch", "hamming") |>
    tidyr::pivot_wider(names_from = "att", values_from = "mismatch")
}
