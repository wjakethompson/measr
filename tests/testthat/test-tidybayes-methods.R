test_that("tidy_draws.measrfit", {
  dina_mcmc <- readRDS("C:/Users/n883j701/Desktop/GitHub-Repos/measrV1/tests/dina_mcmc.rds")
   expect_s4_class(dina_mcmc$model, "stanfit")  # Remove comments after the model is saved as part of the internal package data.
  expect_s3_class(tidy_draws.measrfit(dina_mcmc), c("tbl_df", 'tbl', 'data.frame'))
})
