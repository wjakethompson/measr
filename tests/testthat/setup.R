dina_spec <- dcmstan::dcm_specify(
  qmatrix = q_matrix[, -1],
  measurement_model = dina()
)

# estimate dino with misspecified Q-matrix for items 1 & 3 (over) and
# items 5 & 9 (under)
dino_spec <- dcmstan::dcm_specify(
  qmatrix = dplyr::mutate(
    q_matrix,
    att1 = dplyr::case_when(.data$item == "item_1" ~ 1L, .default = .data$att1),
    att2 = dplyr::case_when(.data$item == "item_3" ~ 1L, .default = .data$att2),
    att4 = dplyr::case_when(.data$item == "item_9" ~ 0L, .default = .data$att4),
    att5 = dplyr::case_when(.data$item == "item_5" ~ 0L, .default = .data$att5),
  ),
  identifier = "item",
  measurement_model = dino()
)

out <- capture.output(
  suppressMessages(
    rstn_dina <- dcm_estimate(
      dina_spec,
      data = dina_data,
      identifier = "resp_id",
      method = "optim",
      backend = "rstan",
      seed = 63277,
      precompiled = stanmodels$test_dina
    )
  )
)

out <- capture.output(
  suppressMessages(
    rstn_dino <- dcm_estimate(
      dino_spec,
      data = dino_data,
      identifier = "resp_id",
      method = "optim",
      backend = "rstan",
      seed = 63277,
      precompiled = stanmodels$test_dina
    )
  )
)
