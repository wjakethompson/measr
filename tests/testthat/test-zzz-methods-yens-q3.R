test_that("Yen's Q works", {
  mod <- add_respondent_estimates(rstn_dina)
  yens_output <- yens_q(mod)

  expect_equal(nrow(yens_output), 595)
  expect_equal(ncol(yens_output), 4)
  expect_equal(names(yens_output), c("item_id", "item_id_2", "resid_corr",
                                     "flag"))

  mod2 <- add_respondent_estimates(rstn_dino)
  yens_output2 <- yens_q(mod2)

  expect_equal(nrow(yens_output2), 595)
  expect_equal(ncol(yens_output2), 4)
  expect_equal(names(yens_output2), c("item_id", "item_id_2", "resid_corr",
                                     "flag"))
})
