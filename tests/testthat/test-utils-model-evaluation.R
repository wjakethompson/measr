test_that("aic works", {
  num_params <- 101
  logLik <- -18474.98

  exp_aic <- (-2 * logLik) + (2 * num_params)
  aic_val <- aic(rstn_dino)

  expect_equal(exp_aic, aic_val)
})

test_that("bic works", {
  num_params <- 101
  N <- 1000
  logLik <- -18474.98

  exp_bic <- (-2 * logLik) + (log(N) * num_params)
  bic_val <- bic(rstn_dino)

  expect_equal(exp_bic, bic_val)
})
