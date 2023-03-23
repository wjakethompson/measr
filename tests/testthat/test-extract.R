test_that("errors for unknown", {
  err <- rlang::catch_cnd(measr_extract(rstn_mdm_lcdm, "tswift"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Cannot extract element `tswift`")
})

test_that("extract item parameters", {
  lcdm_param <- measr_extract(rstn_mdm_lcdm, "item_param")
  expect_equal(nrow(lcdm_param), 8)
  expect_equal(colnames(lcdm_param),
               c("item", "class", "attributes", "coef", "estimate"))
  expect_equal(as.character(lcdm_param$item),
               rep(paste0("mdm", 1:4), each = 2))
  expect_equal(lcdm_param$class,
               rep(c("intercept", "maineffect"), 4))
  expect_equal(lcdm_param$attributes,
               rep(c(NA, "multiplication"), 4))
  expect_equal(lcdm_param$coef,
               dplyr::pull(get_parameters(mdm_qmatrix, item_id = "item",
                                          type = "lcdm"), "coef"))
  expect_s3_class(lcdm_param$estimate, "rvar")
  expect_true(all(!is.na(lcdm_param$estimate)))

  dina_param <- measr_extract(rstn_dina, "item_param")
  expect_equal(nrow(dina_param), 70)
  expect_equal(colnames(dina_param),
               c("item", "class", "coef", "estimate"))
  expect_true(all(dina_param$item %in% q_matrix$item))
  expect_equal(dina_param$class,
               rep(c("slip", "guess"), 35))
  expect_equal(dina_param$coef,
               dplyr::pull(get_parameters(q_matrix, item_id = "item",
                                          type = "dina"), "coef"))
  expect_s3_class(dina_param$estimate, "rvar")
  expect_true(all(!is.na(dina_param$estimate)))
})

test_that("extract structural parameters", {
  lcdm_param <- measr_extract(rstn_mdm_lcdm, "strc_param")
  expect_equal(nrow(lcdm_param), 2)
  expect_equal(lcdm_param$class, dplyr::pull(profile_labels(1), "class"))
  expect_s3_class(lcdm_param$estimate, "rvar")
  expect_true(all(!is.na(lcdm_param$estimate)))

  dina_param <- measr_extract(rstn_dina, "strc_param")
  expect_equal(nrow(dina_param), 32)
  expect_equal(dina_param$class, dplyr::pull(profile_labels(5), "class"))
  expect_s3_class(dina_param$estimate, "rvar")
  expect_true(all(!is.na(dina_param$estimate)))
})

test_that("extract priors", {
  lcdm_param <- measr_extract(rstn_mdm_lcdm, "prior")
  expect_equal(lcdm_param,
               c(prior(uniform(-15, 15), class = "intercept"),
                 prior(uniform(0, 15), class = "maineffect"),
                 prior(uniform(-15, 15), class = "interaction")))

  dina_param <- measr_extract(rstn_dina, "prior")
  expect_equal(dina_param, default_dcm_priors(type = "dina"))
})
