test_that("Q-matrix validation works for ecpe", {
  cmds_ecpe_lcdm <- add_respondent_estimates(cmds_ecpe_lcdm)
  tmp_mod <- add_qmatrix_validation(mod = cmds_ecpe_lcdm)

  expect_equal(names(tmp_mod$qmatrix_validation),
               c("item_id", "validation_flag", "original_specification",
                 "empirical_specification", "pvaf"))
  expect_equal(nrow(tmp_mod$qmatrix_validation), 28)
  expect_equal(nrow(tmp_mod$qmatrix_validation %>%
                      filter(!validation_flag)),
               28)
})
