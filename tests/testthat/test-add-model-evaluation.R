test_that("model evaluation works", {
  dir <- withr::local_tempdir()

  dina_spec <- dcmstan::dcm_specify(
    qmatrix = dcmdata::mdm_qmatrix,
    identifier = "item",
    measurement_model = dina()
  )
  suppressMessages(
    dina_mod <- dcm_estimate(
      dina_spec,
      data = dcmdata::mdm_data,
      identifier = "respondent",
      method = "optim",
      backend = "rstan",
      seed = 63277,
      file = paste0(dir, "/dina")
    )
  )
  expect_true(fs::file_exists(paste0(dir, "/dina.rds")))

  # add evaluation pieces ------------------------------------------------------
  expect_true(rlang::is_empty(dina_mod@criteria$aic))
  expect_true(rlang::is_empty(dina_mod@reliability))
  expect_true(rlang::is_empty(dina_mod@fit$m2))
  expect_true(rlang::is_empty(dina_mod@respondent_estimates))

  err <- rlang::catch_cnd(add_criterion(dina_mod, criterion = "loo"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "must be a model estimated with.*\"mcmc\"")

  err <- rlang::catch_cnd(add_criterion(dina_mod, criterion = "waic"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "must be a model estimated with.*\"mcmc\"")

  dina_mod <- add_criterion(dina_mod, criterion = "aic")
  expect_false(rlang::is_empty(dina_mod@criteria$aic))

  dina_mod <- add_reliability(dina_mod)
  expect_false(rlang::is_empty(dina_mod@reliability))

  err <- rlang::catch_cnd(add_fit(
    dina_mod,
    method = "ppmc",
    model_fit = "raw_score"
  ))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "use `method = \"mcmc\"` for PPMC")

  dina_mod <- add_fit(dina_mod, method = "m2")
  expect_false(rlang::is_empty(dina_mod@fit$m2))

  dina_mod <- add_respondent_estimates(dina_mod)
  expect_false(rlang::is_empty(dina_mod@respondent_estimates))

  # read model -----------------------------------------------------------------
  new_mod <- dcm_estimate(
    dina_spec,
    data = dcmdata::mdm_data,
    identifier = "respondent",
    method = "optim",
    backend = "rstan",
    seed = 63277,
    file = paste0(dir, "/dina"),
    file_refit = "never"
  )
  expect_equal(dina_mod, new_mod)

  new_mod2 <- dcm_estimate(
    dina_spec,
    data = dcmdata::mdm_data,
    identifier = "respondent",
    method = "optim",
    backend = "rstan",
    seed = 63277,
    file = paste0(dir, "/dina"),
    file_refit = "on_change"
  )
  expect_equal(dina_mod, new_mod2)
})
