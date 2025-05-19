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
  lcdm_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
                          measurement_model = lcdm(),
                          identifier = "item")
  dina_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
                           measurement_model = dina(),
                           identifier = "item")

  out <- capture.output(
    suppressMessages(
      rstn_mdm_lcdm <- dcm_estimate(lcdm_spec,
                                    data = dcmdata::mdm_data,
                                    identifier = "respondent",
                                    backend = "rstan", method = "mcmc")
    )
  )

  out <- capture.output(
    suppressMessages(
      rstn_mdm_dina <- dcm_estimate(dina_spec,
                                    data = dcmdata::mdm_data,
                                    identifier = "respondent",
                                    backend = "rstan", method = "mcmc")
    )
  )
}

test_that("Bayes factor works", {
  tmp_mod <- add_bayes_factor(rstn_mdm_lcdm, rstn_mdm_dina)

  expect_equal(length(tmp_mod@bayes_factor), 3)
  expect_equal(names(tmp_mod@bayes_factor),
               c("bf", "comp_model", "posterior_probability"))
  expect_equal(typeof(tmp_mod@bayes_factor$bf), "double")
  expect_equal(tmp_mod@bayes_factor$comp_model, "dina")
  expect_equal(class(tmp_mod@bayes_factor$posterior_probability),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(tmp_mod@bayes_factor$posterior_probability), 10)
  expect_equal(nrow(tmp_mod@bayes_factor$posterior_probability), 2)
  expect_equal(tmp_mod@bayes_factor$posterior_probability$model[1],
               rstn_mdm_lcdm@model_spec@measurement_model@model)
  expect_equal(tmp_mod@bayes_factor$posterior_probability$model[2],
               rstn_mdm_dina@model_spec@measurement_model@model)
  expect_equal(sum(tmp_mod@bayes_factor$posterior_probability[,2]), 1)
  expect_equal(sum(tmp_mod@bayes_factor$posterior_probability[,3]), 1)
  expect_equal(sum(tmp_mod@bayes_factor$posterior_probability[,4]), 1)
  expect_equal(sum(tmp_mod@bayes_factor$posterior_probability[,5]), 1)
  expect_equal(sum(tmp_mod@bayes_factor$posterior_probability[,6]), 1)
  expect_equal(sum(tmp_mod@bayes_factor$posterior_probability[,7]), 1)
  expect_equal(sum(tmp_mod@bayes_factor$posterior_probability[,8]), 1)
  expect_equal(sum(tmp_mod@bayes_factor$posterior_probability[,9]), 1)
  expect_equal(sum(tmp_mod@bayes_factor$posterior_probability[,10]), 1)
})

test_that("Add marginal likelihood works", {
  tmp_mod2 <- add_marginal_likelihood(rstn_mdm_lcdm)

  expect_equal(typeof(tmp_mod2@log_marginal_likelihood$logml), "double")
  expect_equal(length(tmp_mod2@log_marginal_likelihood$logml), 1)
})
