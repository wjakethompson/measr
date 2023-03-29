if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()

out <- capture.output(
  suppressMessages(
    cmds_mdm_lcdm <- measr_dcm(
      data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
      resp_id = "respondent", item_id = "item", type = "lcdm",
      method = "mcmc", seed = 63277, backend = "cmdstanr",
      iter_sampling = 500, iter_warmup = 1000, chains = 2,
      parallel_chains = 2,
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
      cores = 2,
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
  err <- rlang::catch_cnd(loo(rstn_dina))
  expect_s3_class(err, "error_bad_method")
  expect_match(err$message, "`method = \"mcmc\"`")

  err <- rlang::catch_cnd(waic(rstn_dino))
  expect_s3_class(err, "error_bad_method")
  expect_match(err$message, "`method = \"mcmc\"`")

  check_loo <- loo(cmds_mdm_lcdm)
  expect_s3_class(check_loo, "psis_loo")

  check_waic <- waic(cmds_mdm_lcdm)
  expect_s3_class(check_waic, "waic")
})

test_that("loo and waic can be added to model", {
  err <- rlang::catch_cnd(add_criterion(rstn_dino))
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

test_that("ppmc works", {
  test_ppmc <- fit_ppmc(cmds_mdm_lcdm, ndraws = 500, return_draws = 0.2,
                        model_fit = "raw_score",
                        item_fit = "conditional_prob")
  expect_equal(names(test_ppmc), c("model_fit", "item_fit"))
  expect_equal(names(test_ppmc$model_fit), "raw_score")
  expect_s3_class(test_ppmc$model_fit$raw_score, "tbl_df")
  expect_equal(nrow(test_ppmc$model_fit$raw_score), 1L)
  expect_equal(colnames(test_ppmc$model_fit$raw_score),
               c("obs_chisq", "ppmc_mean", "2.5%", "97.5%", "samples", "ppp"))
  expect_equal(length(test_ppmc$model_fit$raw_score$samples[[1]]), 100)

  expect_equal(names(test_ppmc$item_fit), "conditional_prob")
  expect_s3_class(test_ppmc$item_fit$conditional_prob, "tbl_df")
  expect_equal(nrow(test_ppmc$item_fit$conditional_prob), 8L)
  expect_equal(colnames(test_ppmc$item_fit$conditional_prob),
               c("item", "class", "obs_cond_pval", "ppmc_mean", "2.5%", "97.5%",
                 "samples", "ppp"))
  expect_equal(as.character(test_ppmc$item_fit$conditional_prob$item),
               rep(paste0("mdm", 1:4), each = 2))
  expect_equal(as.character(test_ppmc$item_fit$conditional_prob$class),
               rep(c("[0]", "[1]"), 4))
  expect_equal(vapply(test_ppmc$item_fit$conditional_prob$samples,
                      length, integer(1)),
               rep(100, 8))


  test_ppmc <- fit_ppmc(cmds_mdm_lcdm, ndraws = 200, return_draws = 0.9,
                        probs = c(0.055, 0.945),
                        model_fit = NULL, item_fit = "odds_ratio")
  expect_equal(names(test_ppmc), c("item_fit"))
  expect_equal(names(test_ppmc$item_fit), "odds_ratio")
  expect_s3_class(test_ppmc$item_fit$odds_ratio, "tbl_df")
  expect_equal(nrow(test_ppmc$item_fit$odds_ratio), 6L)
  expect_equal(colnames(test_ppmc$item_fit$odds_ratio),
               c("item_1", "item_2", "obs_or", "ppmc_mean", "5.5%", "94.5%",
                 "samples", "ppp"))
  expect_equal(as.character(test_ppmc$item_fit$odds_ratio$item_1),
               c(rep("mdm1", 3), rep("mdm2", 2), "mdm3"))
  expect_equal(as.character(test_ppmc$item_fit$odds_ratio$item_2),
               c("mdm2", "mdm3", "mdm4", "mdm3", "mdm4", "mdm4"))
  expect_equal(vapply(test_ppmc$item_fit$odds_ratio$samples,
                      length, integer(1)),
               rep(180, 6))

  test_ppmc <- fit_ppmc(cmds_mdm_lcdm, ndraws = 1, return_draws = 0,
                        model_fit = "raw_score",
                        item_fit = c("conditional_prob", "odds_ratio"))
  expect_equal(names(test_ppmc), c("model_fit", "item_fit"))
  expect_equal(names(test_ppmc$model_fit), "raw_score")
  expect_equal(colnames(test_ppmc$model_fit$raw_score),
               c("obs_chisq", "ppmc_mean", "2.5%", "97.5%", "ppp"))
  expect_equal(names(test_ppmc$item_fit), c("conditional_prob", "odds_ratio"))
  expect_equal(colnames(test_ppmc$item_fit$conditional_prob),
               c("item", "class", "obs_cond_pval", "ppmc_mean", "2.5%", "97.5%",
                 "ppp"))
  expect_equal(colnames(test_ppmc$item_fit$odds_ratio),
               c("item_1", "item_2", "obs_or", "ppmc_mean", "2.5%", "97.5%",
                 "ppp"))
})

test_that("ppmc extraction errors", {
  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "odds_ratio"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be added")

  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "odds_ratio_flags"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be added")
})

test_that("model fit can be added", {
  test_model <- cmds_mdm_dina
  expect_equal(test_model$fit, list())

  # add m2 and ppmc odds ratios
  test_model <- add_fit(test_model, method = c("m2", "ppmc"),
                        model_fit = NULL, item_fit = "odds_ratio")
  expect_equal(names(test_model$fit), c("m2", "ppmc"))
  expect_equal(names(test_model$fit$ppmc), "item_fit")
  expect_equal(names(test_model$fit$ppmc$item_fit), "odds_ratio")
  expect_equal(names(test_model$fit$ppmc$item_fit$odds_ratio),
               c("item_1", "item_2", "obs_or", "ppmc_mean", "2.5%", "97.5%",
                 "ppp"))

  # now add ppmc raw score and conditional probs -- other fit should persist
  test_model <- add_fit(test_model, method = "ppmc",
                        model_fit = "raw_score", item_fit = "conditional_prob",
                        probs = c(0.055, 0.945))
  expect_equal(names(test_model$fit), c("m2", "ppmc"))
  expect_equal(names(test_model$fit$ppmc), c("item_fit", "model_fit"))
  expect_equal(names(test_model$fit$ppmc$model_fit), "raw_score")
  expect_equal(names(test_model$fit$ppmc$model_fit$raw_score),
               c("obs_chisq", "ppmc_mean", "5.5%", "94.5%", "ppp"))
  expect_equal(names(test_model$fit$ppmc$item_fit),
               c("odds_ratio", "conditional_prob"))
  expect_equal(names(test_model$fit$ppmc$item_fit$odds_ratio),
               c("item_1", "item_2", "obs_or", "ppmc_mean", "2.5%", "97.5%",
                 "ppp"))
  expect_equal(names(test_model$fit$ppmc$item_fit$conditional_prob),
               c("item", "class", "obs_cond_pval", "ppmc_mean", "5.5%", "94.5%",
                 "ppp"))

  # overwrite just odds ration with sample and new probs
  test_model <- add_fit(test_model, method = "ppmc", overwrite = TRUE,
                        model_fit = NULL, item_fit = "odds_ratio",
                        return_draws = 0.2, probs = c(.1, .9))
  expect_equal(names(test_model$fit), c("m2", "ppmc"))
  expect_equal(names(test_model$fit$ppmc), c("item_fit", "model_fit"))
  expect_equal(names(test_model$fit$ppmc$model_fit), "raw_score")
  expect_equal(names(test_model$fit$ppmc$model_fit$raw_score),
               c("obs_chisq", "ppmc_mean", "5.5%", "94.5%", "ppp"))
  expect_equal(names(test_model$fit$ppmc$item_fit),
               c("odds_ratio", "conditional_prob"))
  expect_equal(names(test_model$fit$ppmc$item_fit$odds_ratio),
               c("item_1", "item_2", "obs_or", "ppmc_mean", "10%", "90%",
                 "samples", "ppp"))
  expect_equal(names(test_model$fit$ppmc$item_fit$conditional_prob),
               c("item", "class", "obs_cond_pval", "ppmc_mean", "5.5%", "94.5%",
                 "ppp"))

  # test extraction
  or_check <- measr_extract(test_model, "odds_ratio")
  expect_equal(or_check,
               test_model$fit$ppmc$item_fit$odds_ratio)
  expect_equal(measr_extract(test_model, "odds_ratio_flags",
                             ppmc_interval = 0.95),
               dplyr::filter(or_check, ppp <= 0.025 | ppp >= 0.975))
  expect_equal(measr_extract(test_model, "odds_ratio_flags",
                             ppmc_interval = 0.8),
               dplyr::filter(or_check, ppp <= 0.1 | ppp >= 0.9))
})

test_that("respondent probabilities are correct", {
  mdm_preds <- predict(cmds_mdm_lcdm, newdata = mdm_data,
                       resp_id = "respondent", summary = TRUE)

  # dimensions are correct
  expect_equal(names(mdm_preds), c("class_probabilities",
                                   "attribute_probabilities"))
  expect_equal(colnames(mdm_preds$class_probabilities),
               c("respondent", "class", "probability", "2.5%", "97.5%"))
  expect_equal(colnames(mdm_preds$attribute_probabilities),
               c("respondent", "attribute", "probability", "2.5%", "97.5%"))
  expect_equal(nrow(mdm_preds$class_probabilities),
               nrow(mdm_data) * (2 ^ 1))
  expect_equal(nrow(mdm_preds$attribute_probabilities),
               nrow(mdm_data) * 1)

  # extract works
  expect_equal(cmds_mdm_lcdm$respondent_estimates, list())
  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "class_prob"))
  expect_match(err$message,
               "added to a model object before class probabilities")
  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "attribute_prob"))
  expect_match(err$message,
               "added to a model object before attribute probabilities")

  cmds_mdm_lcdm <- add_respondent_estimates(cmds_mdm_lcdm)
  expect_equal(cmds_mdm_lcdm$respondent_estimates, mdm_preds)
  expect_equal(measr_extract(cmds_mdm_lcdm, "class_prob"),
               mdm_preds$class_probabilities %>%
                 dplyr::select("respondent", "class", "probability") %>%
                 tidyr::pivot_wider(names_from = "class",
                                    values_from = "probability"))
  expect_equal(measr_extract(cmds_mdm_lcdm, "attribute_prob"),
               mdm_preds$attribute_prob %>%
                 dplyr::select("respondent", "attribute", "probability") %>%
                 tidyr::pivot_wider(names_from = "attribute",
                                    values_from = "probability"))
})
