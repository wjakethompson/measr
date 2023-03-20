skip_on_cran()

out <- capture.output(
  suppressMessages(
    cmds_mdm_lcdm <- measr_dcm(
      data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
      resp_id = "respondent", item_id = "item", type = "lcdm",
      method = "mcmc", seed = 63277, backend = "cmdstanr",
      iter_sampling = 500, iter_warmup = 1000, chains = 2,
      parallel_chains = 2, return_stanfit = FALSE,
      prior = c(prior(uniform(-15, 15), class = "intercept"),
                prior(uniform(0, 15), class = "maineffect"),
                prior(uniform(-15, 15), class = "interaction")))
  )
)

out <- capture.output(
  suppressMessages(
    cmds_mdm_dina <- measr_dcm(
      data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
      resp_id = "respondent", item_id = "item", type = "dina",
      method = "mcmc", seed = 63277, backend = "rstan",
      iter = 1500, warmup = 1000, chains = 2,
      cores = 2, return_stanfit = FALSE,
      prior = c(prior(beta(5, 17), class = "slip"),
                prior(beta(5, 17), class = "guess")))
  )
)

test_that("as_draws works", {
  draws <- as_draws(cmds_mdm_dina)
  expect_s3_class(draws, "draws_array")

  draws_a <- posterior::as_draws_array(cmds_mdm_dina)
  expect_s3_class(draws_a, "draws_array")

  draws_d <- posterior::as_draws_df(cmds_mdm_dina)
  expect_s3_class(draws_d, "draws_df")

  draws_l <- posterior::as_draws_list(cmds_mdm_lcdm)
  expect_s3_class(draws_l, "draws_list")

  draws_m <- posterior::as_draws_matrix(cmds_mdm_lcdm)
  expect_s3_class(draws_m, "draws_matrix")

  draws_r <- posterior::as_draws_rvars(cmds_mdm_lcdm)
  expect_s3_class(draws_r, "draws_rvars")
})

test_that("get_mcmc_draws works as expected", {
  test_draws <- get_mcmc_draws(cmds_mdm_lcdm)
  expect_equal(posterior::ndraws(test_draws), 1000)
  expect_equal(posterior::nvariables(test_draws), 10)
  expect_s3_class(test_draws, "draws_array")

  test_draws <- get_mcmc_draws(cmds_mdm_dina, ndraws = 750)
  expect_equal(posterior::ndraws(test_draws), 750)
  expect_equal(posterior::nvariables(test_draws), 10)
  expect_s3_class(test_draws, "draws_array")
})

test_that("log_lik is calculated correctly", {
  log_lik <- prep_loglik_array(cmds_mdm_lcdm)

  # expected value from 2-class LCA fit in Mplus
  expect_equal(sum(apply(log_lik, c(3), mean)), -331.764, tolerance = 1.000)
})

test_that("loo and waic work", {
  err <- rlang::catch_cnd(loo(rstn_mdm_lcdm))
  expect_s3_class(err, "error_bad_method")
  expect_match(err$message, "`method = \"mcmc\"`")

  err <- rlang::catch_cnd(waic(rstn_mdm_lcdm))
  expect_s3_class(err, "error_bad_method")
  expect_match(err$message, "`method = \"mcmc\"`")

  check_loo <- loo(cmds_mdm_lcdm)
  expect_s3_class(check_loo, "psis_loo")

  check_waic <- waic(cmds_mdm_lcdm)
  expect_s3_class(check_waic, "waic")
})

test_that("loo and waic can be added to model", {
  err <- rlang::catch_cnd(add_criterion(rstn_mdm_lcdm))
  expect_s3_class(err, "error_bad_method")
  expect_match(err$message, "`method = \"mcmc\"`")

  loo_model <- add_criterion(cmds_mdm_lcdm, criterion = "loo")
  expect_equal(names(loo_model$criteria), "loo")
  expect_s3_class(loo_model$criteria$loo, "psis_loo")

  lw_model <- add_criterion(loo_model, criterion = c("loo", "waic"),
                            overwrite = TRUE)
  expect_equal(names(lw_model$criteria), c("loo", "waic"))
  expect_s3_class(lw_model$criteria$loo, "psis_loo")
  expect_s3_class(lw_model$criteria$waic, "waic")
  expect_identical(loo_model$criteria$loo, lw_model$criteria$loo)
})

test_that("model comparisons work", {
  err <- rlang::catch_cnd(loo_compare(cmds_mdm_lcdm, cmds_mdm_dina))
  expect_s3_class(err, "error_missing_criterion")
  expect_match(err$message, "does not contain a precomputed")

  lcdm_compare <- add_criterion(cmds_mdm_lcdm, criterion = c("loo", "waic"))
  err <- rlang::catch_cnd(loo_compare(lcdm_compare, cmds_mdm_dina))
  expect_s3_class(err, "error_missing_criterion")
  expect_match(err$message, "Model 2 does not contain a precomputed")

  dina_compare <- add_criterion(cmds_mdm_dina, criterion = c("loo", "waic"))

  err <- rlang::catch_cnd(loo_compare(lcdm_compare, cmds_mdm_dina,
                                      model_names = c("m1", "m2", "m3")))
  expect_s3_class(err, "error_bad_argument")
  expect_match(err$message, "same as the number of models")

  loo_comp <- loo_compare(lcdm_compare, dina_compare, criterion = "loo")
  expect_s3_class(loo_comp, "compare.loo")
  expect_equal(rownames(loo_comp), c("lcdm_compare", "dina_compare"))
  expect_equal(colnames(loo_comp),
               c("elpd_diff", "se_diff", "elpd_loo", "se_elpd_loo",
                 "p_loo", "se_p_loo", "looic", "se_looic"))

  waic_comp <- loo_compare(lcdm_compare, dina_compare, criterion = "waic",
                           model_names = c("first_model", "second_model"))
  expect_s3_class(waic_comp, "compare.loo")
  expect_equal(rownames(waic_comp), c("first_model", "second_model"))
  expect_equal(colnames(waic_comp),
               c("elpd_diff", "se_diff", "elpd_waic", "se_elpd_waic",
                 "p_waic", "se_p_waic", "waic", "se_waic"))
})

test_that("m2 works", {
  m2 <- fit_m2(cmds_mdm_lcdm, ci = 0.9)
  expect_equal(m2$m2, 0.0168, tolerance = 0.1)
  expect_equal(m2$df, 1)
  expect_equal(m2$pval, 0.8968, tolerance = 0.1)
  expect_equal(m2$rmsea, 0, tolerance = 0.1)
  expect_equal(m2$ci_lower, 0, tolerance = 0.1)
  expect_equal(m2$ci_upper, 0.1013, tolerance = 0.1)
  expect_equal(m2$srmsr, 0.0375, tolerance = 0.1)
})

test_that("ppmc works", {

})
