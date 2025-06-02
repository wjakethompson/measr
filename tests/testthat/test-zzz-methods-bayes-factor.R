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
