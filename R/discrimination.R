#' Title
#'
#' @param model The estimated model to be evaluated.
#'
#' @return
#' @export
#' @examplesIf measr_examples()
#' rstn_ecpe_lcdm <- measr_dcm(
#'   data = ecpe_data, missing = NA, qmatrix = ecpe_qmatrix,
#'   resp_id = "resp_id", item_id = "item_id", type = "lcdm",
#'   method = "optim", seed = 63277, backend = "rstan"
#' )
#'
#' cdi(rstn_ecpe_lcdm)
cdi <- function(model) {
  stan_draws <- switch(model$method,
                       "mcmc" = get_mcmc_draws(model),
                       "optim" = get_optim_draws(model))

  pi_matrix <- stan_draws %>%
    posterior::subset_draws(variable = "pi") %>%
    posterior::as_draws_df() %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(cols = -c(".chain", ".iteration", ".draw")) %>%
    dplyr::summarize(value = mean(value), .by = "name") %>%
    tidyr::separate_wider_regex(
      cols = "name",
      patterns = c("pi\\[", item = "[0-9]*", ",", class = "[0-9]*", "\\]")
    ) %>%
    dplyr::mutate(item = as.integer(item), class = as.integer(class))

  hamming <- profile_hamming(
    dplyr::select(measr_extract(model, "classes"), -"class")
  )
  att_names <- hamming %>%
    dplyr::select(-c("profile_1", "profile_2", "hamming")) %>%
    colnames()

  item_discrim <- tidyr::crossing(item = unique(pi_matrix$item),
                                  profile_1 = unique(pi_matrix$class),
                                  profile_2 = unique(pi_matrix$class)) %>%
    dplyr::left_join(pi_matrix, by = c("item", "profile_1" = "class"),
                     relationship = "many-to-one") %>%
    dplyr::rename("prob_1" = "value") %>%
    dplyr::left_join(pi_matrix, by = c("item", "profile_2" = "class"),
                     relationship = "many-to-one") %>%
    dplyr::rename("prob_2" = "value") %>%
    dplyr::mutate(kli = (.data$prob_1 * log(.data$prob_1 / .data$prob_2)) +
                    ((1 - .data$prob_1) *
                       log((1 - .data$prob_1) / (1 - .data$prob_2)))) %>%
    dplyr::left_join(hamming, by = c("profile_1", "profile_2"),
                     relationship = "many-to-one") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.logical),
                                \(x) {
                                  dplyr::case_when(
                                    x & .data$hamming == 1L ~ TRUE,
                                    .default = NA
                                  )
                                }),
                  dplyr::across(dplyr::where(is.logical),
                                \(x) as.integer(x) * .data$kli)) %>%
    dplyr::filter(.data$hamming > 0) %>%
    dplyr::summarize(overall = sum(.data$kli * (1 / .data$hamming)) /
                       sum((1 / hamming)),
                     dplyr::across(dplyr::all_of(att_names),
                                   \(x) mean(x, na.rm = TRUE)),
                     .by = "item")

  test_descrim <- item_discrim %>%
    dplyr::summarize(dplyr::across(-"item", sum))
}

profile_hamming <- function(profiles) {
  profile_combos <- tidyr::crossing(profile_1 = seq_len(nrow(profiles)),
                                    profile_2 = seq_len(nrow(profiles)))


  hamming <- mapply(hamming_distance, profile_combos$profile_1,
                    profile_combos$profile_2,
                    MoreArgs = list(profiles = profiles),
                    SIMPLIFY = FALSE, USE.NAMES = FALSE) %>%
    dplyr::bind_rows()

  dplyr::bind_cols(profile_combos, hamming)
}

hamming_distance <- function(prof1, prof2, profiles) {
  pattern1 <- profiles[prof1, ]
  pattern2 <- profiles[prof2, ]

  pattern1 %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "att", values_to = "patt1") %>%
    dplyr::left_join(tidyr::pivot_longer(pattern2, cols = dplyr::everything(),
                                         names_to = "att", values_to = "patt2"),
                     by = "att", relationship = "one-to-one") %>%
    dplyr::mutate(mismatch = .data$patt1 != .data$patt2,
                  hamming = sum(.data$mismatch)) %>%
    dplyr::select("att", "mismatch", "hamming") %>%
    tidyr::pivot_wider(names_from = "att", values_from = "mismatch")
}
