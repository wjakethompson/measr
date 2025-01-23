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
