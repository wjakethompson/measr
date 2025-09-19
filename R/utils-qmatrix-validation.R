calc_sigma <- function(att_names, q, strc_param, pi_mat, ii) {
  # count the number of measured attributes for Q-matrix specification q
  atts_meas <- sum(q)

  # reduce Q-matrix entry q to only the measured attributes
  filtered_q <- q |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "att",
      values_to = "meas"
    ) |>
    dplyr::filter(.data$meas == 1) |>
    tidyr::pivot_wider(names_from = "att", values_from = "meas")

  # create set possible Q-matrix specifications from the measured attributes
  poss_subsets <- create_profiles(atts_meas)
  colnames(poss_subsets) <- colnames(filtered_q)

  # numbering possible Q-matrix specifications
  poss_subsets <- poss_subsets |>
    tibble::rowid_to_column("subset_prof_num")

  # Aggregate the structural parameters within each possible profile
  # based on the measured attributes. For example, structural parameters for
  # profiles where none of the measured attributes were mastered will be
  # combined under the profile subset where none of the measured
  # attributes were mastered (e.g., 000 and 001 are aggregated within 00 when
  # the first and second attributes are measured)
  condensed_strc_param <- strc_param |>
    tidyr::separate(col = "class", into = att_names) |>
    dplyr::left_join(
      poss_subsets |>
        dplyr::mutate(dplyr::across(
          dplyr::where(is.numeric),
          ~ as.character(.x)
        )),
      by = colnames(filtered_q)
    ) |>
    dplyr::group_by(.data$subset_prof_num) |>
    dplyr::summarize(w = sum(.data$estimate)) |>
    dplyr::ungroup()

  # calculate the weighted p-value, aggregated within the subset of each of the
  # possible profiles based on the measured attributes
  condensed_p <- strc_param |>
    tidyr::separate(col = "class", into = att_names) |>
    tibble::rowid_to_column("prof_num") |>
    dplyr::left_join(
      pi_mat |>
        dplyr::filter(.data$item_id == ii) |>
        dplyr::select(-"item_id"),
      by = c("prof_num" = "profile_id")
    ) |>
    dplyr::select(-"prof_num") |>
    dplyr::left_join(
      poss_subsets |>
        dplyr::mutate(dplyr::across(
          dplyr::where(is.numeric),
          ~ as.character(.x)
        )),
      by = colnames(filtered_q)
    ) |>
    dplyr::mutate(product = .data$estimate * .data$prob) |>
    dplyr::group_by(.data$subset_prof_num) |>
    dplyr::summarize(
      product_sum = sum(.data$product),
      w_sum = sum(.data$estimate)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(p = .data$product_sum / .data$w_sum) |>
    dplyr::select("subset_prof_num", "p")

  # calculate components for sigma as described by de la Torre & Chiu (2016)
  sigma_components <- condensed_p |>
    dplyr::left_join(condensed_strc_param, by = "subset_prof_num") |>
    dplyr::mutate(wp = .data$w * .data$p, wp2 = .data$w * .data$p * .data$p) |>
    dplyr::summarize(p_bar = sum(.data$wp), wp2 = sum(.data$wp2))

  # extract components for sigma calculation
  ## weighted p-value
  p_bar <- sigma_components |>
    dplyr::pull(p_bar)
  ## aggregated structural parameter times weighted p-value squared
  wp2 <- sigma_components |>
    dplyr::pull(wp2)

  # calculate sigma
  sigma <- wp2 - (p_bar^2)

  return(sigma)
}
