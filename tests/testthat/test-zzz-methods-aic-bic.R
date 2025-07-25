test_that("aic and bic work", {
  num_params <- 101
  n <- 1000
  dino_log_lik <- -18474.98
  dina_log_lik <- -18663.8

  expect_equal((-2 * dina_log_lik) + (2 * num_params),
               aic(rstn_dina), tolerance = 0.01)
  expect_equal((-2 * dina_log_lik) + (log(n) * num_params),
               bic(rstn_dina), tolerance = 0.01)
  expect_equal((-2 * dino_log_lik) + (2 * num_params), aic(rstn_dino),
               tolerance = 0.01)
  expect_equal((-2 * dino_log_lik) + (log(n) * num_params), bic(rstn_dino),
               tolerance = 0.01)

  expect_equal(aic(rstn_dina), 37529.60, tolerance = 0.01)
  expect_equal(bic(rstn_dina), 38025.28, tolerance = 0.01)
  expect_equal(aic(rstn_dino), 37151.96, tolerance = 0.01)
  expect_equal(bic(rstn_dino), 37647.64, tolerance = 0.01)
})

test_that("store aic and bic", {
  # dina aic -------------------------------------------------------------------
  expect_true(rlang::is_empty(rstn_dina@criteria$aic))
  expect_true(rlang::is_empty(rstn_dina@criteria$bic))

  err <- rlang::catch_cnd(measr_extract(rstn_dina, "aic"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "criterion\\nmust be added to a model object")

  dina_aic <- add_criterion(rstn_dina, "aic")
  expect_false(rlang::is_empty(dina_aic@criteria$aic))
  expect_true(rlang::is_empty(dina_aic@criteria$bic))
  expect_identical(aic(rstn_dina), dina_aic@criteria$aic)
  expect_identical(aic(dina_aic), dina_aic@criteria$aic)
  expect_identical(measr_extract(dina_aic, "aic"), aic(rstn_dina))

  # dino bic -------------------------------------------------------------------
  expect_true(rlang::is_empty(rstn_dino@criteria$aic))
  expect_true(rlang::is_empty(rstn_dino@criteria$bic))

  err <- rlang::catch_cnd(measr_extract(rstn_dino, "bic"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "criterion\\nmust be added to a model object")

  dino_bic <- add_criterion(rstn_dino, "bic")
  expect_false(rlang::is_empty(dino_bic@criteria$bic))
  expect_true(rlang::is_empty(dino_bic@criteria$aic))
  expect_identical(bic(rstn_dino), dino_bic@criteria$bic)
  expect_identical(bic(dino_bic), dino_bic@criteria$bic)
  expect_identical(measr_extract(dino_bic, "bic"), bic(rstn_dino))

  dino_bic <- add_criterion(dino_bic, c("aic", "bic"), overwrite = TRUE)
  expect_false(rlang::is_empty(dino_bic@criteria$bic))
  expect_false(rlang::is_empty(dino_bic@criteria$aic))
  expect_identical(bic(rstn_dino), dino_bic@criteria$bic)
  expect_identical(bic(dino_bic), dino_bic@criteria$bic)
  expect_identical(aic(rstn_dino), dino_bic@criteria$aic)
  expect_identical(aic(dino_bic), dino_bic@criteria$aic)

  expect_equal(measr_extract(dino_bic, "aic"), 37151.96, tolerance = .0001)
  expect_equal(measr_extract(dino_bic, "bic"), 37647.64, tolerance = .0001)
})
