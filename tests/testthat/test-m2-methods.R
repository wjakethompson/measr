test_that("m2 works", {
  m2 <- fit_m2(rstn_dina, ci = 0.8)
  expect_equal(m2$m2, 552.6549, tolerance = 0.1)
  expect_equal(m2$df, 529)
  expect_equal(m2$pval, 0.2305, tolerance = 0.1)
  expect_equal(m2$rmsea, 0.0067, tolerance = 0.1)
  expect_equal(m2$ci_lower, 0, tolerance = 0.1)
  expect_equal(m2$ci_upper, 0.0115, tolerance = 0.1)
  expect_equal(m2$srmsr, 0.0301, tolerance = 0.1)

  err <- rlang::catch_cnd(measr_extract(rstn_dina, "m2"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be added")

  m2_mod <- add_fit(rstn_dina, method = "m2", ci = 0.8)
  expect_equal(m2_mod$fit$m2, m2)
  expect_equal(measr_extract(m2_mod, "m2", quiet = TRUE), m2)
  extract_check <- utils::capture.output(
    m2_check <- measr_extract(m2_mod, "m2")
  )
  expect_equal(m2_check, m2)
  expect_equal(length(extract_check), 3)
  expect_match(extract_check[1], "M2 =")
  expect_match(extract_check[2], "RMSEA =")
  expect_match(extract_check[3], "SRMSR =")



  m2 <- fit_m2(rstn_dino, ci = 0.95)
  expect_equal(m2$m2, 565.0893, tolerance = 0.1)
  expect_equal(m2$df, 529)
  expect_equal(m2$pval, 0.1344, tolerance = 0.1)
  expect_equal(m2$rmsea, 0.0083, tolerance = 0.1)
  expect_equal(m2$ci_lower, 0, tolerance = 0.1)
  expect_equal(m2$ci_upper, 0.0144, tolerance = 0.1)
  expect_equal(m2$srmsr, 0.031, tolerance = 0.1)

  err <- rlang::catch_cnd(measr_extract(rstn_dino, "m2"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Model fit information must be added")

  m2_mod <- add_fit(rstn_dino, method = "m2", ci = 0.95)
  expect_equal(m2_mod$fit$m2, m2)
  expect_equal(measr_extract(m2_mod, "m2", quiet = TRUE), m2)
  extract_check <- utils::capture.output(
    m2_check <- measr_extract(m2_mod, "m2")
  )
  expect_equal(m2_check, m2)
  expect_equal(length(extract_check), 3)
  expect_match(extract_check[1], "M2 =")
  expect_match(extract_check[2], "RMSEA =")
  expect_match(extract_check[3], "SRMSR =")
})
