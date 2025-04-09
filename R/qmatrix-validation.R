library(tidyverse)
library(measr)

cmds_ecpe_lcdm <- measr_dcm(
  data = ecpe_data, missing = NA, qmatrix = ecpe_qmatrix,
  resp_id = "resp_id", item_id = "item_id", type = "lcdm",
  method = "optim", seed = 63277, backend = "cmdstanr",
  prior = c(prior(uniform(-15, 15), class = "intercept"),
            prior(uniform(0, 15), class = "maineffect"),
            prior(uniform(-15, 15), class = "interaction")))

mod <- cmds_ecpe_lcdm

mod <- add_respondent_estimates(mod)
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
  left_join(item_ids, by = "item_id") |>
  dplyr::mutate(item_id = .data$new_item_id) |>
  dplyr::select(-"new_item_id")

qmatrix <- mod$data$qmatrix
qmatrix <- qmatrix %>%
  dplyr::select(-!!dplyr::sym(mod$data$item_id))

# posterior probabilities of each class
strc_param <- measr_extract(mod, "strc_param")
strc_param <- strc_param %>%
  dplyr::mutate(estimate = mean(.data$estimate)) |>
  dplyr::select("class", "estimate") |>
  dplyr::mutate(# class = gsub(",", "", class),
                class = sub("\\[", "", class),
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

calc_sigma <- function(q, strc_param, pi_mat) {
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
    dplyr::group_by(prof_num) |>
    dplyr::summarize(w = sum(.data$estimate)) |>
    dplyr::ungroup()

  condensed_p <- strc_param |>
    tidyr::separate(col = "class", into = colnames(q)) |>
    tibble::rowid_to_column("prof_num") |>
    dplyr::left_join(pi_mat |>
                       dplyr::filter(item_id == ii) |>
                       dplyr::select(-"item_id"),
                     by = c("prof_num" = "profile_id")) |>
    dplyr::select(-"prof_num") |>
    dplyr::left_join(poss_subsets |>
                       dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                                   ~ as.character(.x))),
                     by = colnames(filtered_q)) |>
    dplyr::mutate(product = .data$estimate * .data$prob) |>
    dplyr::group_by(prof_num) |>
    dplyr::summarize(product_sum = sum(.data$product),
                     w_sum = sum(.data$estimate)) |>
    dplyr::ungroup() |>
    dplyr::mutate(p = .data$product_sum / .data$w_sum) |>
    dplyr::select("prof_num", "p")

  # calculate sigma_1:K
  sigma_components <- condensed_p |>
    dplyr::left_join(condensed_strc_param, by = "prof_num") |>
    dplyr::mutate(wp = w * p,
                  wp2 = w * p * p) |>
    dplyr::summarize(p_bar = sum(.data$wp),
                     wp2 = sum(.data$wp2))

  p_bar <- sigma_components |>
    dplyr::pull(p_bar)
  wp2 <- sigma_components |>
    dplyr::pull(wp2)

  sigma <- wp2 - (p_bar^2)

  return(sigma)
}

colnames(qmatrix) <- colnames(all_profiles)

validation_output <- tibble::tibble()

# calculate sigma_1:K* (e.g., sigma_1:2)
for (ii in seq_len(nrow(qmatrix))) {
  max_specification <- all_profiles[nrow(all_profiles),]
  max_sigma <- calc_sigma(q = max_specification, strc_param = strc_param,
                          pi_mat = pi_mat)

  max_specification <- max_specification |>
    dplyr::mutate(pvaf = max_sigma / max_sigma)

  possible_specifications <- tibble()
  possible_specifications <- bind_rows(possible_specifications,
                                       max_specification)

  # predefine epsilon
    # de la Torre & Chiu used .95
  epsilon <- .95

  for (jj in 2:(nrow(all_profiles) - 1)) {
    q <- all_profiles[jj, ]
    sigma_q <- calc_sigma(q = q, strc_param = strc_param, pi_mat = pi_mat)

    # calculate sigma / sigma_1:K (i.e., PVAF)
    pvaf <- sigma_q / max_sigma
    q <- q |>
      dplyr::mutate(pvaf = pvaf)

    # flagging profiles where sigma / sigma_1:K < epsilon
    # only profiles where sigma / sigma_1:K >= epsilon are appropriate
    keep_spec <- sigma_q / max_sigma > epsilon
    if (keep_spec) {
      possible_specifications <- bind_rows(possible_specifications, q)
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
                                actual_specification = list(actual_spec),
                                empirical_specification = list(correct_spec),
                                pvaf = final_pvaf)
  validation_output <- bind_rows(validation_output, item_output)
}








