test_that("add criterion error messages work", {
  err <- rlang::catch_cnd(add_criterion("test"))
  expect_s3_class(err, "error_bad_argument")
  expect_match(err$message, "must be an object with class measrfit")


  err <- rlang::catch_cnd(add_criterion(rstn_dino, criterion = "waic"))
  expect_s3_class(err, "error_bad_method")
  expect_match(err$message, "LOO and WAIC model criteria are only available")

  err <- rlang::catch_cnd(add_criterion(rstn_dino, criterion = "waic"))
  expect_s3_class(err, "error_bad_method")
  expect_match(err$message, "LOO and WAIC model criteria are only available")

  test_dino <- rstn_dino
  test_dino$method <- "mcmc"
  err <- rlang::catch_cnd(add_criterion(test_dino, criterion = "aic"))
  expect_s3_class(err, "error_bad_method")
  expect_match(err$message, "AIC and BIC model criteria are only available")

  err <- rlang::catch_cnd(add_criterion(test_dino, criterion = "bic"))
  expect_s3_class(err, "error_bad_method")
  expect_match(err$message, "AIC and BIC model criteria are only available")
})

test_that("AIC works", {
  rstn_dino <- add_criterion(rstn_dino, criterion = "aic")

  exp_aic <- 37151.96

  expect_equal(rstn_dino$criteria$aic, exp_aic, tolerance = .0001)
})

test_that("BIC works", {
  rstn_dino <- add_criterion(rstn_dino, criterion = "bic")

  exp_bic <- 37647.64

  expect_equal(rstn_dino$criteria$bic, exp_bic, tolerance = .0001)
})

if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  skip("No MCMC on CRAN")
} else {
  out <- capture.output(
    suppressMessages(
      cmds_mdm_lcdm <- measr_dcm(
        data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
        resp_id = "respondent", item_id = "item", type = "lcdm",
        method = "mcmc", seed = 63277, backend = "cmdstanr",
        iter_sampling = 500, iter_warmup = 1000, chains = 2,
        parallel_chains = 2,
        prior = c(prior(uniform(-15, 15), class = "intercept"),
                  prior(uniform(0, 15), class = "maineffect")))
    )
  )

  out <- capture.output(
    suppressMessages(
      cmds_mdm_dina <- measr_dcm(
        data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
        resp_id = "respondent", item_id = "item", type = "dina",
        attribute_structure = "independent",
        method = "mcmc", seed = 63277, backend = "rstan",
        iter = 1500, warmup = 1000, chains = 2,
        cores = 2, refresh = 0,
        prior = c(prior(beta(5, 17), class = "slip"),
                  prior(beta(5, 17), class = "guess")))
    )
  )
}

test_that("Bayes factor works", {
  tmp_mod <- add_bayes_factor(cmds_mdm_lcdm, cmds_mdm_dina)

  expect_equal(length(tmp_mod$bayes_factor), 3)
  expect_equal(names(tmp_mod$bayes_factor),
               c("bf", "comp_model", "posterior_probability"))
  expect_equal(typeof(tmp_mod$bayes_factor$bf), "double")
  expect_equal(tmp_mod$bayes_factor$comp_model, "dina")
  expect_equal(class(tmp_mod$bayes_factor$posterior_probability),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(tmp_mod$bayes_factor$posterior_probability), 10)
  expect_equal(nrow(tmp_mod$bayes_factor$posterior_probability), 2)
  expect_equal(tmp_mod$bayes_factor$posterior_probability$model[1],
               cmds_mdm_lcdm$type)
  expect_equal(tmp_mod$bayes_factor$posterior_probability$model[2],
               cmds_mdm_lcdm$cmds_mdm_dina)
  expect_equal(sum(tmp_mod$bayes_factor$posterior_probability[,2]), 1)
  expect_equal(sum(tmp_mod$bayes_factor$posterior_probability[,3]), 1)
  expect_equal(sum(tmp_mod$bayes_factor$posterior_probability[,4]), 1)
  expect_equal(sum(tmp_mod$bayes_factor$posterior_probability[,5]), 1)
  expect_equal(sum(tmp_mod$bayes_factor$posterior_probability[,6]), 1)
  expect_equal(sum(tmp_mod$bayes_factor$posterior_probability[,7]), 1)
  expect_equal(sum(tmp_mod$bayes_factor$posterior_probability[,8]), 1)
  expect_equal(sum(tmp_mod$bayes_factor$posterior_probability[,9]), 1)
  expect_equal(sum(tmp_mod$bayes_factor$posterior_probability[,10]), 1)
})

test_that("Add marginal likelihood works", {
  tmp_mod2 <- add_marginal_likelihood(cmds_mdm_lcdm)

  expect_equal(typeof(tmp_mod2$log_marginal_likelihood), "double")
  expect_equal(length(tmp_mod2$log_marginal_likelihood), 1)
})
