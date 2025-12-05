test_that("measrdcm creation works", {
  # dina test ------------------------------------------------------------------
  expect_s7_class(rstn_dina, measrfit)
  expect_s7_class(rstn_dina, measrdcm)
  expect_identical(rstn_dina@model_spec@qmatrix, dina_spec@qmatrix)
  expect_identical(
    rstn_dina@model_spec@qmatrix_meta$attribute_names,
    dina_spec@qmatrix_meta$attribute_names
  )
  expect_identical(rstn_dina@model_spec@qmatrix_meta$item_identifier, "item_id")
  expect_identical(
    rstn_dina@model_spec@qmatrix_meta$item_names,
    rlang::set_names(1:35, paste0("item_", 1:35))
  )
  expect_identical(
    rstn_dina@model_spec@measurement_model,
    dina_spec@measurement_model
  )
  expect_identical(
    rstn_dina@model_spec@structural_model,
    dina_spec@structural_model
  )
  expect_identical(rstn_dina@model_spec@priors, dina_spec@priors)

  expect_identical(
    rstn_dina@data$item_identifier,
    rstn_dina@model_spec@qmatrix_meta$item_identifier
  )
  expect_identical(
    rstn_dina@data$item_names,
    rstn_dina@model_spec@qmatrix_meta$item_names
  )

  expect_s3_class(rstn_dina@stancode, "glue")
  expect_s7_class(rstn_dina@method, optim)
  expect_s7_class(rstn_dina@backend, rstan)
  expect_true(is.list(rstn_dina@model))
  expect_true(
    is.list(rstn_dina@respondent_estimates) &&
      rlang::is_empty(rstn_dina@respondent_estimates)
  )
  expect_true(is.list(rstn_dina@fit) && rlang::is_empty(rstn_dina@fit))
  expect_true(
    is.list(rstn_dina@criteria) &&
      rlang::is_empty(rstn_dina@criteria)
  )
  expect_true(
    is.list(rstn_dina@reliability) &&
      rlang::is_empty(rstn_dina@reliability)
  )
  expect_true(is.character(rstn_dina@file) && rlang::is_empty(rstn_dina@file))
  expect_identical(
    names(rstn_dina@version),
    c("R", "R-measr", "R-rstan", "R-StanHeaders", "Stan")
  )

  dina_comp <- get_draws(rstn_dina, vars = c("Vc", "slip", "guess")) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    dplyr::select(-c(".chain", ".iteration", ".draw")) |>
    tidyr::pivot_longer(cols = everything()) |>
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) |>
    dplyr::left_join(true_dinoa, by = c("name" = "param"))

  comp_cor <- cor(dina_comp$value, dina_comp$true)
  comp_dif <- abs(dina_comp$value - dina_comp$true)

  expect_gte(comp_cor, 0.85)
  expect_lte(max(comp_dif), 0.2)

  # dino test ------------------------------------------------------------------
  expect_s7_class(rstn_dino, measrfit)
  expect_s7_class(rstn_dino, measrdcm)
  expect_identical(rstn_dino@model_spec, dino_spec)
  expect_identical(
    rstn_dino@data$item_identifier,
    rstn_dino@model_spec@qmatrix_meta$item_identifier
  )
  expect_identical(
    rstn_dino@data$item_names,
    rstn_dino@model_spec@qmatrix_meta$item_names
  )

  expect_s3_class(rstn_dino@stancode, "glue")
  expect_s7_class(rstn_dino@method, optim)
  expect_s7_class(rstn_dino@backend, rstan)
  expect_true(is.list(rstn_dino@model))
  expect_true(
    is.list(rstn_dino@respondent_estimates) &&
      rlang::is_empty(rstn_dino@respondent_estimates)
  )
  expect_true(is.list(rstn_dino@fit) && rlang::is_empty(rstn_dino@fit))
  expect_true(
    is.list(rstn_dino@criteria) &&
      rlang::is_empty(rstn_dino@criteria)
  )
  expect_true(
    is.list(rstn_dino@reliability) &&
      rlang::is_empty(rstn_dino@reliability)
  )
  expect_true(is.character(rstn_dino@file) && rlang::is_empty(rstn_dino@file))
  expect_identical(
    names(rstn_dino@version),
    c("R", "R-measr", "R-rstan", "R-StanHeaders", "Stan")
  )

  dino_comp <- get_draws(rstn_dino, vars = c("Vc", "slip", "guess")) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    dplyr::select(-c(".chain", ".iteration", ".draw")) |>
    tidyr::pivot_longer(cols = everything()) |>
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) |>
    dplyr::left_join(true_dinoa, by = c("name" = "param"))

  comp_cor <- cor(dino_comp$value, dino_comp$true)
  comp_dif <- abs(dino_comp$value - dino_comp$true)

  expect_gte(comp_cor, 0.85)
  expect_lte(max(comp_dif), 0.2)
})

test_that("measrdcm setters work", {
  new_fit <- measrfit()

  # @model_spec ----------------------------------------------------------------
  new_fit@model_spec <- rstn_dina@model_spec
  expect_identical(new_fit@model_spec, rstn_dina@model_spec)
  expect_error(
    {
      new_fit@model_spec <- dino_spec
    },
    "@model_spec is read-only"
  )

  # @data ----------------------------------------------------------------------
  expect_error(
    {
      new_fit@data <- rstn_dina@data
    },
    "@data is read-only"
  )

  # @stancode ------------------------------------------------------------------
  expect_error(
    {
      new_fit@stancode <- rstn_dina@stancode
    },
    "@stancode is read-only"
  )

  # @method --------------------------------------------------------------------
  expect_error(
    {
      new_fit@method <- rstn_dina@method
    },
    "@method is read-only"
  )

  # @algorithm -----------------------------------------------------------------
  expect_error(
    {
      new_fit@algorithm <- rstn_dina@algorithm
    },
    "@algorithm is read-only"
  )

  # @backend -------------------------------------------------------------------
  expect_identical(new_fit@backend, stanbackend())
  expect_error(
    {
      new_fit@backend <- rstn_dina@backend
    },
    "@backend is read-only"
  )

  # @model ---------------------------------------------------------------------
  expect_error(
    {
      new_fit@model <- rstn_dina@model
    },
    "@model is read-only"
  )

  # @file ----------------------------------------------------------------------
  expect_error(
    {
      new_fit@file <- "my/path"
    },
    "@file is read-only"
  )

  # @version -------------------------------------------------------------------
  expect_error(
    {
      new_fit@version <- "0.0.0.9000"
    },
    "@version is read-only"
  )
})

test_that("validator works", {
  expect_error(
    {
      measrfit(backend = rstan(), method = mcmc(), model = rstn_dina@model)
    },
    "@model must be a .*stanfit.* object"
  )

  expect_error(
    {
      measrfit(backend = cmdstanr(), method = optim(), model = rstn_dina@model)
    },
    "@model must be a .*CmdStanMLE.* object"
  )

  expect_error(
    {
      measrfit(backend = cmdstanr(), method = mcmc(), model = rstn_dina@model)
    },
    "@model must be a .*CmdStanMCMC.* object"
  )
})
