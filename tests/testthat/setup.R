out <- capture.output(
  suppressMessages(
    rstn_mdm_lcdm <- measr_dcm(data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
                          resp_id = "respondent", item_id = "item", type = "lcdm",
                          method = "optim", seed = 63277, backend = "rstan",
                          prior = c(prior(uniform(-15, 15), class = "intercept"),
                                    prior(uniform(0, 15), class = "maineffect"),
                                    prior(uniform(-15, 15), class = "interaction")))
  )
)

out <- capture.output(
  suppressMessages(
    rstn_ecpe_lcdm <- measr_dcm(data = ecpe_data, missing = NA, qmatrix = ecpe_qmatrix,
                                resp_id = "resp_id", item_id = "item_id", type = "lcdm",
                                method = "optim", seed = 63277, backend = "rstan",
                                prior = c(prior(uniform(-15, 15), class = "intercept"),
                                          prior(uniform(0, 15), class = "maineffect"),
                                          prior(uniform(-15, 15), class = "interaction")))
  )
)

out <- capture.output(
  suppressMessages(
    rstn_dina <- measr_dcm(data = dina_data, missing = NA, qmatrix = q_matrix,
                           resp_id = "resp_id", item_id = "item", type = "dina",
                           method = "optim", seed = 63277, backend = "rstan")
  )
)

out <- capture.output(
  suppressMessages(
    rstn_dino <- measr_dcm(data = dino_data, missing = NA, qmatrix = q_matrix,
                           resp_id = "resp_id", item_id = "item", type = "dino",
                           method = "optim", seed = 63277, backend = "rstan")
  )
)
