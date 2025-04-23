test_that("Q-matrix validation works for ecpe", {
  out <- capture.output(
    suppressMessages(
      cmds_ecpe_lcdm <- measr_dcm(
        data = ecpe_data, missing = NA, qmatrix = ecpe_qmatrix,
        resp_id = "resp_id", item_id = "item_id", type = "lcdm",
        method = "optim", seed = 63277, backend = "cmdstanr",
        prior = c(prior(uniform(-15, 15), class = "intercept"),
                  prior(uniform(0, 15), class = "maineffect"),
                  prior(uniform(-15, 15), class = "interaction")))
    )
  )
  cmds_ecpe_lcdm <- add_respondent_estimates(cmds_ecpe_lcdm)
  tmp_mod <- add_qmatrix_validation(mod = cmds_ecpe_lcdm)

  expect_equal(names(tmp_mod$qmatrix_validation),
               c("item_id", "validation_flag", "original_specification",
                 "empirical_specification", "pvaf"))
  expect_equal(nrow(tmp_mod$qmatrix_validation), 28)
  expect_equal(nrow(tmp_mod$qmatrix_validation %>%
                      dplyr::filter(!validation_flag)),
               28)
})
