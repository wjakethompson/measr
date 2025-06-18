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

  rstn_mdm_lcdm <- add_respondent_estimates(rstn_mdm_lcdm)
  rstn_mdm_dina <- add_respondent_estimates(rstn_mdm_dina)
}

test_that("Bayes factor works", {
  bf_res <- bayes_factor(rstn_mdm_lcdm, rstn_mdm_dina)

  expect_equal(length(bf_res), 3)
  expect_equal(names(bf_res),
               c("bf", "comp_model", "posterior_probability"))
  expect_equal(typeof(bf_res$bf), "double")
  expect_equal(bf_res$comp_model, "dina")
  expect_equal(class(bf_res$posterior_probability),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(bf_res$posterior_probability), 10)
  expect_equal(nrow(bf_res$posterior_probability), 2)
  expect_equal(bf_res$posterior_probability$model[1],
               rstn_mdm_lcdm@model_spec@measurement_model@model)
  expect_equal(bf_res$posterior_probability$model[2],
               rstn_mdm_dina@model_spec@measurement_model@model)
  expect_equal(sum(bf_res$posterior_probability[, 2]), 1)
  expect_equal(sum(bf_res$posterior_probability[, 3]), 1)
  expect_equal(sum(bf_res$posterior_probability[, 4]), 1)
  expect_equal(sum(bf_res$posterior_probability[, 5]), 1)
  expect_equal(sum(bf_res$posterior_probability[, 6]), 1)
  expect_equal(sum(bf_res$posterior_probability[, 7]), 1)
  expect_equal(sum(bf_res$posterior_probability[, 8]), 1)
  expect_equal(sum(bf_res$posterior_probability[, 9]), 1)
  expect_equal(sum(bf_res$posterior_probability[, 10]), 1)
})

test_that("Add marginal likelihood works", {
  mll_res <- add_marginal_likelihood(rstn_mdm_lcdm)

  expect_equal(typeof(mll_res), "double")
  expect_equal(length(mll_res), 1)
})
