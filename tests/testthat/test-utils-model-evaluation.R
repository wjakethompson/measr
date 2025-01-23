test_that("aic works", {
  num_params <- 101
  log_lik <- -18474.98

  exp_aic <- (-2 * log_lik) + (2 * num_params)
  aic_val <- aic(rstn_dino)

  expect_equal(exp_aic, aic_val)
})

test_that("bic works", {
  num_params <- 101
  n <- 1000
  log_lik <- -18474.98

  exp_bic <- (-2 * log_lik) + (log(n) * num_params)
  bic_val <- bic(rstn_dino)

  expect_equal(exp_bic, bic_val)
})
