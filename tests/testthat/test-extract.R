test_that("errors for unknown", {
  err <- rlang::catch_cnd(measr_extract(rstn_dina, "tswift"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Cannot extract element `tswift`")
})

test_that("extract item parameters", {
  dina_param <- measr_extract(rstn_dina, "item_param")
  expect_equal(nrow(dina_param), 70)
  expect_equal(colnames(dina_param),
               c("item", "class", "coef", "estimate"))
  expect_true(all(dina_param$item %in% q_matrix$item))
  expect_equal(dina_param$class,
               rep(c("slip", "guess"), 35))
  expect_equal(dina_param$coef,
               get_parameters(q_matrix, item_id = "item", type = "dina") %>%
                 dplyr::filter(class != "structural") %>%
                 dplyr::pull("coef"))
  expect_s3_class(dina_param$estimate, "rvar")
  expect_true(all(!is.na(dina_param$estimate)))
})

test_that("extract structural parameters", {
  dina_param <- measr_extract(rstn_dina, "strc_param")
  expect_equal(nrow(dina_param), 32)
  expect_equal(dina_param$class, dplyr::pull(profile_labels(5), "class"))
  expect_s3_class(dina_param$estimate, "rvar")
  expect_true(all(!is.na(dina_param$estimate)))
})

test_that("extract priors", {
  dina_param <- measr_extract(rstn_dina, "prior")
  expect_equal(dina_param, default_dcm_priors(type = "dina"))
})

test_that("extract classes", {
  dino_param <- measr_extract(rstn_dino, "classes")
  expect_equal(colnames(dino_param), c("class", paste0("att", 1:5)))
  expect_equal(dino_param$class, dplyr::pull(profile_labels(5), "class"))

  exp_label <- dino_param %>%
    dplyr::mutate(new_label = paste0("[", att1, ",", att2, ",", att3, ",",
                                     att4, ",", att5, "]")) %>%
    dplyr::pull("new_label")
  expect_equal(dino_param$class, exp_label)
})
