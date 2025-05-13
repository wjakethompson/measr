test_that("measrdcm creation works", {
  # dina test ------------------------------------------------------------------
  S7::check_is_S7(rstn_dina, measrfit)
  S7::check_is_S7(rstn_dina, measrdcm)
  expect_identical(rstn_dina@model_spec@qmatrix, dina_spec@qmatrix)
  expect_identical(rstn_dina@model_spec@qmatrix_meta$attribute_names,
                   dina_spec@qmatrix_meta$attribute_names)
  expect_identical(rstn_dina@model_spec@qmatrix_meta$item_identifier,
                   "item_id")
  expect_identical(rstn_dina@model_spec@qmatrix_meta$item_names,
                   rlang::set_names(1:35, paste0("item_", 1:35)))
  expect_identical(rstn_dina@model_spec@measurement_model,
                   dina_spec@measurement_model)
  expect_identical(rstn_dina@model_spec@structural_model,
                   dina_spec@structural_model)
  expect_identical(rstn_dina@model_spec@priors, dina_spec@priors)

  expect_identical(rstn_dina@data$item_identifier,
                   rstn_dina@model_spec@qmatrix_meta$item_identifier)
  expect_identical(rstn_dina@data$item_names,
                   rstn_dina@model_spec@qmatrix_meta$item_names)

  expect_s3_class(rstn_dina@stancode, "glue")
  expect_true(S7::S7_inherits(rstn_dina@method, optim))
  expect_true(S7::S7_inherits(rstn_dina@backend, rstan))
  expect_true(is.list(rstn_dina@model))
  expect_true(is.list(rstn_dina@respondent_estimates) &&
                rlang::is_empty(rstn_dina@respondent_estimates))
  expect_true(is.list(rstn_dina@fit) && rlang::is_empty(rstn_dina@fit))
  expect_true(is.list(rstn_dina@criteria) &&
                rlang::is_empty(rstn_dina@criteria))
  expect_true(is.list(rstn_dina@reliability) &&
                rlang::is_empty(rstn_dina@reliability))
  expect_true(is.character(rstn_dina@file) && rlang::is_empty(rstn_dina@file))
  expect_identical(names(rstn_dina@version), c("R", "R-measr", "R-rstan",
                                               "R-StanHeaders", "Stan"))

  # dino test ------------------------------------------------------------------
  S7::check_is_S7(rstn_dino, measrfit)
  S7::check_is_S7(rstn_dino, measrdcm)
  expect_identical(rstn_dino@model_spec, dino_spec)
  expect_identical(rstn_dino@data$item_identifier,
                   rstn_dino@model_spec@qmatrix_meta$item_identifier)
  expect_identical(rstn_dino@data$item_names,
                   rstn_dino@model_spec@qmatrix_meta$item_names)

  expect_s3_class(rstn_dino@stancode, "glue")
  expect_true(S7::S7_inherits(rstn_dino@method, optim))
  expect_true(S7::S7_inherits(rstn_dino@backend, rstan))
  expect_true(is.list(rstn_dino@model))
  expect_true(is.list(rstn_dino@respondent_estimates) &&
                rlang::is_empty(rstn_dino@respondent_estimates))
  expect_true(is.list(rstn_dino@fit) && rlang::is_empty(rstn_dino@fit))
  expect_true(is.list(rstn_dino@criteria) &&
                rlang::is_empty(rstn_dino@criteria))
  expect_true(is.list(rstn_dino@reliability) &&
                rlang::is_empty(rstn_dino@reliability))
  expect_true(is.character(rstn_dino@file) && rlang::is_empty(rstn_dino@file))
  expect_identical(names(rstn_dino@version), c("R", "R-measr", "R-rstan",
                                               "R-StanHeaders", "Stan"))
})
