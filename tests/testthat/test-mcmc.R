if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  skip("No MCMC on CRAN")
} else {
  lcdm_spec <- dcm_specify(
    qmatrix = dcmdata::mdm_qmatrix,
    identifier = "item",
    measurement_model = lcdm(),
    structural_model = unconstrained(),
    priors = c(
      prior(uniform(-15, 15), type = "intercept"),
      prior(uniform(0, 15), type = "maineffect")
    )
  )
  dina_spec <- dcm_specify(
    qmatrix = dcmdata::mdm_qmatrix,
    identifier = "item",
    measurement_model = dina(),
    structural_model = independent(),
    priors = c(
      prior(beta(5, 17), type = "slip"),
      prior(beta(5, 17), type = "guess")
    )
  )

  out <- capture.output(
    suppressMessages(
      cmds_mdm_lcdm <- dcm_estimate(
        lcdm_spec,
        data = dcmdata::mdm_data,
        identifier = "respondent",
        missing = NA,
        method = "mcmc",
        seed = 63277,
        backend = "cmdstanr",
        iter_sampling = 500,
        iter_warmup = 1000,
        chains = 2,
        parallel_chains = 2
      )
    )
  )

  out <- capture.output(
    suppressMessages(
      rstn_mdm_dina <- dcm_estimate(
        dina_spec,
        data = dcmdata::mdm_data,
        identifier = "respondent",
        missing = NA,
        method = "mcmc",
        seed = 63277,
        backend = "rstan",
        iter = 1500,
        warmup = 1000,
        chains = 2,
        cores = 2,
        refresh = 0
      )
    )
  )
}

# draws ------------------------------------------------------------------------
test_that("as_draws works", {
  skip_on_cran()

  draws <- as_draws(rstn_mdm_dina)
  expect_s3_class(draws, "draws_array")

  draws_a <- posterior::as_draws_array(rstn_mdm_dina)
  expect_s3_class(draws_a, "draws_array")

  draws_d <- posterior::as_draws_df(rstn_mdm_dina)
  expect_s3_class(draws_d, "draws_df")

  draws_l <- posterior::as_draws_list(cmds_mdm_lcdm)
  expect_s3_class(draws_l, "draws_list")

  draws_m <- posterior::as_draws_matrix(cmds_mdm_lcdm)
  expect_s3_class(draws_m, "draws_matrix")

  draws_r <- posterior::as_draws_rvars(cmds_mdm_lcdm)
  expect_s3_class(draws_r, "draws_rvars")
})

test_that("get_draws works as expected", {
  skip_on_cran()

  test_draws <- get_draws(cmds_mdm_lcdm)
  expect_equal(posterior::ndraws(test_draws), 1000)
  expect_equal(posterior::nvariables(test_draws), 21)
  expect_s3_class(test_draws, "draws_array")

  test_draws <- get_draws(rstn_mdm_dina, vars = c("log_Vc", "pi"), ndraws = 750)
  expect_equal(posterior::ndraws(test_draws), 750)
  expect_equal(posterior::nvariables(test_draws), 10)
  expect_s3_class(test_draws, "draws_array")
})

# extracts ---------------------------------------------------------------------
test_that("extract pi matrix", {
  lcdm_pimat <- measr_extract(cmds_mdm_lcdm, "pi_matrix")
  expect_equal(nrow(lcdm_pimat), 4)
  expect_equal(ncol(lcdm_pimat), 3)
  expect_equal(lcdm_pimat$item, dcmdata::mdm_qmatrix$item)
  expect_equal(
    colnames(lcdm_pimat)[-1],
    dplyr::pull(profile_labels(1), "class")
  )
  expect_true(all(vapply(lcdm_pimat[, -1], posterior::is_rvar, logical(1))))
  expect_true(all(vapply(lcdm_pimat[, -1], \(x) !any(is.na(x)), logical(1))))
})

test_that("extract model p-values", {
  dina_pimat <- measr_extract(rstn_mdm_dina, "exp_pvalues")
  expect_equal(nrow(dina_pimat), 4)
  expect_equal(ncol(dina_pimat), 4)
  expect_equal(dina_pimat$item, dcmdata::mdm_qmatrix$item)
  expect_equal(
    colnames(dina_pimat)[-1],
    c(dplyr::pull(profile_labels(1), "class"), "overall")
  )
  expect_true(all(vapply(dina_pimat[, -1], posterior::is_rvar, logical(1))))
  expect_true(all(vapply(dina_pimat[, -1], \(x) !any(is.na(x)), logical(1))))
})

# loglik -----------------------------------------------------------------------
test_that("loglik is calculated correctly", {
  skip_on_cran()

  cmds_log_lik <- loglik(cmds_mdm_lcdm)
  rstn_log_lik <- loglik(rstn_mdm_dina)

  # expected value from 2-class LCA fit in Mplus
  expect_equal(cmds_log_lik, -331.764, tolerance = 1.000)
  expect_equal(rstn_log_lik, -331.764, tolerance = 1.000)
})

# loo/waic ---------------------------------------------------------------------
test_that("loo and waic work", {
  skip_on_cran()

  err <- rlang::catch_cnd(loo(rstn_dina))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`method = \"mcmc\"`")

  err <- rlang::catch_cnd(waic(rstn_dino))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`method = \"mcmc\"`")

  check_loo <- loo(cmds_mdm_lcdm)
  expect_s3_class(check_loo, "psis_loo")

  check_waic <- waic(rstn_mdm_dina)
  expect_s3_class(check_waic, "waic")
})

test_that("loo and waic can be added to model", {
  skip_on_cran()

  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "loo"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "LOO criterion\\nmust be added")

  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "waic"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "WAIC criterion\\nmust be added")

  err <- rlang::catch_cnd(add_criterion(rstn_dino))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`method = \"mcmc\"`")

  loo_model <- add_criterion(cmds_mdm_lcdm, criterion = "loo")
  expect_equal(names(loo_model@criteria), "loo")
  expect_s3_class(loo_model@criteria$loo, "psis_loo")

  lw_model <- add_criterion(
    loo_model,
    criterion = c("loo", "waic"),
    overwrite = TRUE
  )
  expect_equal(names(lw_model@criteria), c("loo", "waic"))
  expect_s3_class(lw_model@criteria$loo, "psis_loo")
  expect_s3_class(lw_model@criteria$waic, "waic")
  expect_identical(loo_model@criteria$loo, lw_model@criteria$loo)

  expect_identical(measr_extract(lw_model, "loo"), lw_model@criteria$loo)
  expect_identical(measr_extract(lw_model, "waic"), lw_model@criteria$waic)

  expect_identical(lw_model@criteria$loo, loo(lw_model))
  expect_identical(lw_model@criteria$waic, waic(lw_model))
})

test_that("model comparisons work", {
  skip_on_cran()

  no_save <- loo_compare(cmds_mdm_lcdm, rstn_mdm_dina)
  expect_s3_class(no_save, "compare.loo")
  expect_equal(rownames(no_save), c("cmds_mdm_lcdm", "rstn_mdm_dina"))
  expect_equal(
    colnames(no_save),
    c(
      "elpd_diff",
      "se_diff",
      "elpd_loo",
      "se_elpd_loo",
      "p_loo",
      "se_p_loo",
      "looic",
      "se_looic"
    )
  )

  lcdm_compare <- add_criterion(cmds_mdm_lcdm, criterion = c("loo", "waic"))
  lcdm_save <- loo_compare(lcdm_compare, rstn_mdm_dina)
  expect_s3_class(lcdm_save, "compare.loo")
  expect_equal(rownames(lcdm_save), c("lcdm_compare", "rstn_mdm_dina"))
  expect_equal(
    colnames(lcdm_save),
    c(
      "elpd_diff",
      "se_diff",
      "elpd_loo",
      "se_elpd_loo",
      "p_loo",
      "se_p_loo",
      "looic",
      "se_looic"
    )
  )

  dina_compare <- add_criterion(rstn_mdm_dina, criterion = c("loo", "waic"))
  dina_save <- loo_compare(cmds_mdm_lcdm, dina_compare, criterion = "waic")
  expect_s3_class(dina_save, "compare.loo")
  expect_equal(rownames(dina_save), c("cmds_mdm_lcdm", "dina_compare"))
  expect_equal(
    colnames(dina_save),
    c(
      "elpd_diff",
      "se_diff",
      "elpd_waic",
      "se_elpd_waic",
      "p_waic",
      "se_p_waic",
      "waic",
      "se_waic"
    )
  )

  all_save <- loo_compare(lcdm_compare, dina_compare, criterion = "loo")
  expect_s3_class(all_save, "compare.loo")
  expect_equal(rownames(all_save), c("lcdm_compare", "dina_compare"))
  expect_equal(
    colnames(all_save),
    c(
      "elpd_diff",
      "se_diff",
      "elpd_loo",
      "se_elpd_loo",
      "p_loo",
      "se_p_loo",
      "looic",
      "se_looic"
    )
  )

  err <- rlang::catch_cnd(loo_compare(
    lcdm_compare,
    dina_compare,
    model_names = c("m1", "m2", "m3")
  ))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "same as the number of models")

  err <- rlang::catch_cnd(loo_compare(lcdm_compare, no_save))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "must be a .*measrdcm.* object")

  waic_comp <- loo_compare(
    lcdm_compare,
    dina_compare,
    criterion = "waic",
    model_names = c("first_model", "second_model")
  )
  expect_s3_class(waic_comp, "compare.loo")
  expect_equal(rownames(waic_comp), c("first_model", "second_model"))
  expect_equal(
    colnames(waic_comp),
    c(
      "elpd_diff",
      "se_diff",
      "elpd_waic",
      "se_elpd_waic",
      "p_waic",
      "se_p_waic",
      "waic",
      "se_waic"
    )
  )
})

# ppmc -------------------------------------------------------------------------
test_that("ppmc works", {
  skip_on_cran()

  test_ppmc <- fit_ppmc(cmds_mdm_lcdm)
  expect_equal(test_ppmc, list())

  # test 1 -----
  test_ppmc <- fit_ppmc(
    cmds_mdm_lcdm,
    ndraws = 500,
    return_draws = 100,
    model_fit = "raw_score",
    item_fit = "conditional_prob"
  )
  expect_equal(names(test_ppmc), c("ppmc_raw_score", "ppmc_conditional_prob"))

  expect_s3_class(test_ppmc$ppmc_raw_score, "tbl_df")
  expect_equal(nrow(test_ppmc$ppmc_raw_score), 1L)
  expect_equal(
    colnames(test_ppmc$ppmc_raw_score),
    c(
      "obs_chisq",
      "ppmc_mean",
      "2.5%",
      "97.5%",
      "rawscore_samples",
      "chisq_samples",
      "ppp"
    )
  )
  expect_equal(nrow(test_ppmc$ppmc_raw_score$rawscore_samples[[1]]), 100)
  expect_equal(length(test_ppmc$ppmc_raw_score$chisq_samples[[1]]), 100)

  expect_s3_class(test_ppmc$ppmc_conditional_prob, "tbl_df")
  expect_equal(nrow(test_ppmc$ppmc_conditional_prob), 8L)
  expect_equal(
    colnames(test_ppmc$ppmc_conditional_prob),
    c(
      "item",
      "class",
      "obs_cond_pval",
      "ppmc_mean",
      "2.5%",
      "97.5%",
      "samples",
      "ppp"
    )
  )
  expect_equal(
    as.character(test_ppmc$ppmc_conditional_prob$item),
    rep(paste0("mdm", 1:4), each = 2)
  )
  expect_equal(
    as.character(test_ppmc$ppmc_conditional_prob$class),
    rep(c("[0]", "[1]"), 4)
  )
  expect_equal(
    vapply(test_ppmc$ppmc_conditional_prob$samples, length, integer(1)),
    rep(100, 8)
  )

  # test 2 -----
  test_ppmc <- fit_ppmc(
    rstn_mdm_dina,
    ndraws = 200,
    return_draws = 180,
    probs = c(0.055, 0.945),
    item_fit = c("odds_ratio", "pvalue")
  )
  expect_equal(names(test_ppmc), c("ppmc_odds_ratio", "ppmc_pvalue"))
  expect_s3_class(test_ppmc$ppmc_odds_ratio, "tbl_df")
  expect_equal(nrow(test_ppmc$ppmc_odds_ratio), 6L)
  expect_equal(
    colnames(test_ppmc$ppmc_odds_ratio),
    c(
      "item_1",
      "item_2",
      "obs_or",
      "ppmc_mean",
      "5.5%",
      "94.5%",
      "samples",
      "ppp"
    )
  )
  expect_equal(
    as.character(test_ppmc$ppmc_odds_ratio$item_1),
    c(rep("mdm1", 3), rep("mdm2", 2), "mdm3")
  )
  expect_equal(
    as.character(test_ppmc$ppmc_odds_ratio$item_2),
    c("mdm2", "mdm3", "mdm4", "mdm3", "mdm4", "mdm4")
  )
  expect_equal(
    vapply(test_ppmc$ppmc_odds_ratio$samples, length, integer(1)),
    rep(180, 6)
  )

  expect_s3_class(test_ppmc$ppmc_pvalue, "tbl_df")
  expect_equal(nrow(test_ppmc$ppmc_pvalue), 4)
  expect_equal(
    colnames(test_ppmc$ppmc_pvalue),
    c("item", "obs_pvalue", "ppmc_mean", "5.5%", "94.5%", "samples", "ppp")
  )
  expect_equal(as.character(test_ppmc$ppmc_pvalue$item), paste0("mdm", 1:4))
  expect_equal(
    vapply(test_ppmc$ppmc_pvalue$samples, length, double(1)),
    rep(180, 4)
  )

  # test 3 -----
  test_ppmc <- fit_ppmc(
    cmds_mdm_lcdm,
    ndraws = 1,
    return_draws = 0,
    model_fit = "raw_score",
    item_fit = c("conditional_prob", "odds_ratio", "pvalue")
  )
  expect_equal(
    names(test_ppmc),
    c(
      "ppmc_raw_score",
      "ppmc_conditional_prob",
      "ppmc_odds_ratio",
      "ppmc_pvalue"
    )
  )
  expect_equal(
    colnames(test_ppmc$ppmc_raw_score),
    c("obs_chisq", "ppmc_mean", "2.5%", "97.5%", "ppp")
  )
  expect_equal(
    colnames(test_ppmc$ppmc_conditional_prob),
    c("item", "class", "obs_cond_pval", "ppmc_mean", "2.5%", "97.5%", "ppp")
  )
  expect_equal(
    colnames(test_ppmc$ppmc_odds_ratio),
    c("item_1", "item_2", "obs_or", "ppmc_mean", "2.5%", "97.5%", "ppp")
  )
  expect_equal(
    colnames(test_ppmc$ppmc_pvalue),
    c("item", "obs_pvalue", "ppmc_mean", "2.5%", "97.5%", "ppp")
  )
})

test_that("ppmc extraction errors", {
  skip_on_cran()

  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "ppmc_raw_score"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be\\nadded")

  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "ppmc_conditional_prob"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be\\nadded")

  err <- rlang::catch_cnd(measr_extract(
    cmds_mdm_lcdm,
    "ppmc_conditional_prob_flags"
  ))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be\\nadded")

  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "ppmc_odds_ratio"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be\\nadded")

  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "ppmc_odds_ratio_flags"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be\\nadded")

  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "ppmc_pvalue"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be\\nadded")
})

test_that("model fit can be added", {
  skip_on_cran()

  test_model <- rstn_mdm_dina
  expect_equal(test_model@fit, list())

  # add m2 and ppmc odds ratios -----
  test_model <- add_fit(
    test_model,
    method = c("m2", "ppmc"),
    model_fit = NULL,
    item_fit = "odds_ratio",
    return_draws = 100
  )
  expect_equal(names(test_model@fit), c("m2", "ppmc_odds_ratio"))
  expect_equal(
    names(test_model@fit$ppmc_odds_ratio),
    c(
      "item_1",
      "item_2",
      "obs_or",
      "ppmc_mean",
      "2.5%",
      "97.5%",
      "samples",
      "ppp"
    )
  )
  expect_identical(
    test_model@fit[-which(names(test_model@fit) == "m2")],
    fit_ppmc(test_model, item_fit = "odds_ratio")
  )

  # nothing new does nothing -----
  test_model2 <- add_fit(test_model, method = "ppmc")
  expect_identical(test_model, test_model2)

  # now add raw score and conditional probs -- other fit should persist -----
  test_model <- add_fit(
    test_model,
    method = "ppmc",
    model_fit = "raw_score",
    item_fit = "conditional_prob",
    probs = c(0.055, 0.945)
  )
  expect_equal(
    names(test_model@fit),
    c("m2", "ppmc_odds_ratio", "ppmc_raw_score", "ppmc_conditional_prob")
  )
  expect_equal(
    names(test_model@fit$ppmc_raw_score),
    c("obs_chisq", "ppmc_mean", "5.5%", "94.5%", "ppp")
  )
  expect_equal(
    names(test_model@fit$ppmc_odds_ratio),
    c(
      "item_1",
      "item_2",
      "obs_or",
      "ppmc_mean",
      "2.5%",
      "97.5%",
      "samples",
      "ppp"
    )
  )
  expect_equal(
    names(test_model@fit$ppmc_conditional_prob),
    c("item", "class", "obs_cond_pval", "ppmc_mean", "5.5%", "94.5%", "ppp")
  )

  # now calculate conditional probs and overall pvalue - overall is new, -----
  # but conditional prob should use stored value
  test_ppmc <- fit_ppmc(
    test_model,
    model_fit = NULL,
    item_fit = c("conditional_prob", "pvalue")
  )
  expect_equal(names(test_ppmc), c("ppmc_conditional_prob", "ppmc_pvalue"))
  expect_identical(
    test_ppmc$ppmc_conditional_prob,
    test_model@fit$ppmc_conditional_prob
  )
  expect_equal(
    names(test_ppmc$ppmc_pvalue),
    c("item", "obs_pvalue", "ppmc_mean", "2.5%", "97.5%", "ppp")
  )

  # overwrite just conditional prob with samples and new probs -----
  # add overall p-values
  test_model <- add_fit(
    test_model,
    method = "ppmc",
    overwrite = TRUE,
    model_fit = NULL,
    item_fit = c("conditional_prob", "pvalue"),
    return_draws = 200,
    probs = c(.1, .9)
  )
  expect_equal(
    names(test_model@fit),
    c(
      "m2",
      "ppmc_odds_ratio",
      "ppmc_raw_score",
      "ppmc_conditional_prob",
      "ppmc_pvalue"
    )
  )
  expect_equal(
    names(test_model@fit$ppmc_raw_score),
    c("obs_chisq", "ppmc_mean", "5.5%", "94.5%", "ppp")
  )
  expect_equal(
    names(test_model@fit$ppmc_odds_ratio),
    c(
      "item_1",
      "item_2",
      "obs_or",
      "ppmc_mean",
      "2.5%",
      "97.5%",
      "samples",
      "ppp"
    )
  )
  expect_equal(
    names(test_model@fit$ppmc_conditional_prob),
    c(
      "item",
      "class",
      "obs_cond_pval",
      "ppmc_mean",
      "10%",
      "90%",
      "samples",
      "ppp"
    )
  )
  expect_equal(
    names(test_model@fit$ppmc_pvalue),
    c("item", "obs_pvalue", "ppmc_mean", "10%", "90%", "samples", "ppp")
  )

  # test extraction -----
  rs_check <- measr_extract(test_model, "ppmc_raw_score")
  expect_equal(rs_check, test_model@fit$ppmc_raw_score)

  cp_check <- measr_extract(test_model, "ppmc_conditional_prob")
  expect_equal(cp_check, test_model@fit$ppmc_conditional_prob)
  expect_equal(
    measr_extract(
      test_model,
      "ppmc_conditional_prob_flags",
      ppmc_interval = 0.95
    ),
    dplyr::filter(cp_check, ppp <= 0.025 | ppp >= 0.975)
  )
  expect_equal(
    measr_extract(
      test_model,
      "ppmc_conditional_prob_flags",
      ppmc_interval = 0.8
    ),
    dplyr::filter(cp_check, ppp <= 0.1 | ppp >= 0.9)
  )

  or_check <- measr_extract(test_model, "ppmc_odds_ratio")
  expect_equal(or_check, test_model@fit$ppmc_odds_ratio)
  expect_equal(
    measr_extract(test_model, "ppmc_odds_ratio_flags", ppmc_interval = 0.95),
    dplyr::filter(or_check, ppp <= 0.025 | ppp >= 0.975)
  )
  expect_equal(
    measr_extract(test_model, "ppmc_odds_ratio_flags", ppmc_interval = 0.8),
    dplyr::filter(or_check, ppp <= 0.1 | ppp >= 0.9)
  )

  pval_check <- measr_extract(test_model, "ppmc_pvalue")
  expect_equal(pval_check, test_model@fit$ppmc_pvalue)
  expect_equal(
    measr_extract(test_model, "ppmc_pvalue_flags", ppmc_interval = 0.95),
    dplyr::filter(pval_check, ppp <= 0.025 | ppp >= 0.975)
  )
  expect_equal(
    measr_extract(test_model, "ppmc_pvalue_flags", ppmc_interval = 0.6),
    dplyr::filter(pval_check, ppp <= 0.2 | ppp >= 0.8)
  )
})

# respondent scores ------------------------------------------------------------
test_that("respondent probabilities are correct", {
  skip_on_cran()

  mdm_preds <- score(
    cmds_mdm_lcdm,
    newdata = dcmdata::mdm_data,
    identifier = "respondent",
    summary = TRUE
  )
  mdm_full_preds <- score(cmds_mdm_lcdm, summary = FALSE)

  # dimensions are correct -----
  expect_equal(
    names(mdm_preds),
    c("class_probabilities", "attribute_probabilities")
  )
  expect_equal(
    colnames(mdm_preds$class_probabilities),
    c("respondent", "class", "probability", "2.5%", "97.5%")
  )
  expect_equal(
    colnames(mdm_preds$attribute_probabilities),
    c("respondent", "attribute", "probability", "2.5%", "97.5%")
  )
  expect_equal(
    nrow(mdm_preds$class_probabilities),
    nrow(dcmdata::mdm_data) * (2^1)
  )
  expect_equal(
    nrow(mdm_preds$attribute_probabilities),
    nrow(dcmdata::mdm_data) * 1
  )

  expect_equal(
    names(mdm_full_preds),
    c("class_probabilities", "attribute_probabilities")
  )
  expect_equal(
    colnames(mdm_full_preds$class_probabilities),
    c("respondent", "[0]", "[1]")
  )
  expect_equal(
    colnames(mdm_full_preds$attribute_probabilities),
    c("respondent", "multiplication")
  )
  expect_equal(
    nrow(mdm_full_preds$class_probabilities),
    nrow(dcmdata::mdm_data)
  )
  expect_equal(
    nrow(mdm_full_preds$attribute_probabilities),
    nrow(dcmdata::mdm_data)
  )

  # extract works -----
  expect_equal(cmds_mdm_lcdm@respondent_estimates, list())
  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "class_prob"))
  expect_match(
    err$message,
    "added to a model object before\\nclass probabilities"
  )
  err <- rlang::catch_cnd(measr_extract(cmds_mdm_lcdm, "attribute_prob"))
  expect_match(
    err$message,
    "added to a model object before\\nattribute probabilities"
  )

  cmds_mdm_lcdm <- add_respondent_estimates(cmds_mdm_lcdm)
  expect_equal(cmds_mdm_lcdm@respondent_estimates, mdm_preds)
  expect_equal(
    measr_extract(cmds_mdm_lcdm, "class_prob"),
    mdm_preds$class_probabilities |>
      dplyr::select("respondent", "class", "probability") |>
      tidyr::pivot_wider(names_from = "class", values_from = "probability")
  )
  expect_equal(
    measr_extract(cmds_mdm_lcdm, "attribute_prob"),
    mdm_preds$attribute_prob |>
      dplyr::select("respondent", "attribute", "probability") |>
      tidyr::pivot_wider(names_from = "attribute", values_from = "probability")
  )
})
