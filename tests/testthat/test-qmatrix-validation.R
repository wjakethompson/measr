test_that("Q-matrix validation works for ecpe", {
  mod_spec <- dcm_specify(qmatrix = dcmdata::ecpe_qmatrix,
                          identifier = "item_id")

  out <- capture.output(
    suppressMessages(
      rstn_ecpe_lcdm <- dcm_estimate(mod_spec, data = dcmdata::ecpe_data,
                                     identifier = "resp_id",
                                     backend = "rstan",
                                     method = "optim")
    )
  )
  rstn_ecpe_lcdm <- add_respondent_estimates(rstn_ecpe_lcdm)
  tmp_mod <- add_qmatrix_validation(mod = rstn_ecpe_lcdm)

  expect_equal(names(tmp_mod@qmatrix_validation),
               c("item_id", "validation_flag", "original_specification",
                 "empirical_specification", "pvaf"))
  expect_equal(nrow(tmp_mod@qmatrix_validation), 28)
  expect_equal(nrow(tmp_mod@qmatrix_validation %>%
                      dplyr::filter(!validation_flag)),
               28)
})
