calc_sigma <- function(q, strc_param, pi_mat, ii) {
  atts_meas <- sum(q)
  filtered_q <- q |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "att",
                        values_to = "meas") |>
    dplyr::filter(.data$meas == 1) |>
    tidyr::pivot_wider(names_from = "att", values_from = "meas")

  # iterate through the 2^K - 1 possible classes for each item to calculate sigma (e.g., sigma_1:3)
  poss_subsets <- create_profiles(atts_meas)
  colnames(poss_subsets) <- colnames(filtered_q)
  poss_subsets <- poss_subsets |>
    tibble::rowid_to_column("prof_num")

  condensed_strc_param <- strc_param |>
    tidyr::separate(col = "class", into = colnames(q)) |>
    dplyr::left_join(poss_subsets |>
                       dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                                   ~ as.character(.x))),
                     by = colnames(filtered_q)) |>
    dplyr::group_by(.data$prof_num) |>
    dplyr::summarize(w = sum(.data$estimate)) |>
    dplyr::ungroup()

  condensed_p <- strc_param |>
    tidyr::separate(col = "class", into = colnames(q)) |>
    tibble::rowid_to_column("prof_num") |>
    dplyr::left_join(pi_mat |>
                       dplyr::filter(.data$item_id == ii) |>
                       dplyr::select(-"item_id"),
                     by = c("prof_num" = "profile_id")) |>
    dplyr::select(-"prof_num") |>
    dplyr::left_join(poss_subsets |>
                       dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                                   ~ as.character(.x))),
                     by = colnames(filtered_q)) |>
    dplyr::mutate(product = .data$estimate * .data$prob) |>
    dplyr::group_by(.data$prof_num) |>
    dplyr::summarize(product_sum = sum(.data$product),
                     w_sum = sum(.data$estimate)) |>
    dplyr::ungroup() |>
    dplyr::mutate(p = .data$product_sum / .data$w_sum) |>
    dplyr::select("prof_num", "p")

  # calculate sigma_1:K
  sigma_components <- condensed_p |>
    dplyr::left_join(condensed_strc_param, by = "prof_num") |>
    dplyr::mutate(wp = .data$w * .data$p,
                  wp2 = .data$w * .data$p * .data$p) |>
    dplyr::summarize(p_bar = sum(.data$wp),
                     wp2 = sum(.data$wp2))

  p_bar <- sigma_components |>
    dplyr::pull(p_bar)
  wp2 <- sigma_components |>
    dplyr::pull(wp2)

  sigma <- wp2 - (p_bar^2)

  return(sigma)
}
