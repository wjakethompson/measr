out <- capture.output(
  suppressMessages(
    rstn_dina <- measr_dcm(data = dina_data, missing = NA, qmatrix = q_matrix,
                           resp_id = "resp_id", item_id = "item", type = "dina",
                           method = "optim", seed = 63277, backend = "rstan",
                           precompiled = stanmodels$test_dina)
  )
)

out <- capture.output(
  suppressMessages(
    rstn_dino <- measr_dcm(data = dino_data, missing = NA, qmatrix = q_matrix,
                           resp_id = "resp_id", item_id = "item", type = "dino",
                           method = "optim", seed = 63277, backend = "rstan",
                           precompiled = stanmodels$test_dina)
  )
)
