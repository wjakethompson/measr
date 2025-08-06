dina_spec <- dcmstan::dcm_specify(
  qmatrix = q_matrix[, -1],
  measurement_model = dina()
)

dino_spec <- dcmstan::dcm_specify(
  qmatrix = q_matrix,
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
