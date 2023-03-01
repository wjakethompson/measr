skip_on_cran()

out <- capture.output(
  suppressMessages(
    cmds_mdm_mcmc <- measr_dcm(
      data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
      resp_id = "respondent", item_id = "item", type = "lcdm",
      method = "mcmc", seed = 63277, backend = "cmdstanr",
      iter_sampling = 300, iter_warmup = 400, chains = 2,
      parallel_chains = 2, return_stanfit = FALSE,
      prior = c(prior(uniform(-15, 15), class = "intercept"),
                prior(uniform(0, 15), class = "maineffect"),
                prior(uniform(-15, 15), class = "interaction")))
  )
)

test_that("log_lik is calculated correctly", {
  log_lik <- prep_loglik_array(cmds_mdm_mcmc)

  # expected value from 2-class LCA fit in Mplus
  expect_equal(sum(apply(log_lik, c(3), mean)), -331.764, tolerance = 1.000)
})
