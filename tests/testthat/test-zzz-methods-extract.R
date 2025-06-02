test_that("errors for unknown", {
  err <- rlang::catch_cnd(measr_extract(rstn_dina, "tswift"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Cannot extract element .*\"tswift\".*")
})

test_that("extract item parameters", {
  # dina -----------------------------------------------------------------------
  dina_param <- measr_extract(rstn_dina, "item_param")
  expect_equal(nrow(dina_param), 70)
  expect_equal(colnames(dina_param),
               c("item_id", "type", "coefficient", "estimate"))
  expect_true(all(dina_param$item_id %in% q_matrix$item))
  expect_equal(dina_param$type,
               rep(c("guess", "slip"), 35))
  expect_equal(dina_param$coefficient,
               dcmstan::get_parameters(dina(), qmatrix = q_matrix[, -1]) |>
                 dplyr::pull(coefficient))
  expect_s3_class(dina_param$estimate, "rvar")
  expect_true(all(!is.na(dina_param$estimate)))

  # dino -----------------------------------------------------------------------
  dino_param <- measr_extract(rstn_dino, "item_param")
  expect_equal(nrow(dino_param), 70)
  expect_equal(colnames(dino_param),
               c("item", "type", "coefficient", "estimate"))
  expect_true(all(dino_param$item %in% q_matrix$item))
  expect_equal(dino_param$type,
               rep(c("guess", "slip"), 35))
  expect_equal(dino_param$coefficient,
               dcmstan::get_parameters(dino(), qmatrix = q_matrix,
                                       identifier = "item") |>
                 dplyr::pull(coefficient))
  expect_s3_class(dino_param$estimate, "rvar")
  expect_true(all(!is.na(dino_param$estimate)))
})

test_that("extract structural parameters", {
  dina_param <- measr_extract(rstn_dina, "strc_param")
  expect_equal(nrow(dina_param), 32)
  expect_equal(dina_param$class, dplyr::pull(profile_labels(5), "class"))
  expect_s3_class(dina_param$estimate, "rvar")
  expect_true(all(!is.na(dina_param$estimate)))
})

test_that("extract priors", {
  dino_param <- measr_extract(rstn_dino, "prior")
  expect_equal(dino_param, dcmstan::default_dcm_priors(dino(), unconstrained()))
})

test_that("extract classes", {
  dino_param <- measr_extract(rstn_dino, "classes")
  expect_equal(colnames(dino_param), c("class", paste0("att", 1:5)))
  expect_equal(dino_param$class, dplyr::pull(profile_labels(5), "class"))

  exp_label <- dino_param |>
    dplyr::mutate(new_label = paste0("[", att1, ",", att2, ",", att3, ",",
                                     att4, ",", att5, "]")) |>
    dplyr::pull("new_label")
  expect_equal(dino_param$class, exp_label)
})
