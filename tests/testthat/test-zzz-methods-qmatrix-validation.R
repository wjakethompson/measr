test_that("Q-matrix validation works for ecpe", {
  # correctly specified dina model ---------------------------------------------
  qmat_valid_res <- qmatrix_validation(x = rstn_dina)
  expect_equal(
    names(qmat_valid_res),
    c(
      "item_id",
      "original_specification",
      "original_pvaf",
      "empirical_specification",
      "empirical_pvaf"
    )
  )
  expect_equal(nrow(qmat_valid_res), 35)
  expect_equal(
    nrow(
      qmat_valid_res |>
        dplyr::filter(is.na(empirical_specification))
    ),
    35
  )

  # misspecified dino model ----------------------------------------------------
  qmat_valid_res <- qmatrix_validation(x = rstn_dino)
  expect_equal(
    names(qmat_valid_res),
    c(
      "item_id",
      "original_specification",
      "original_pvaf",
      "empirical_specification",
      "empirical_pvaf"
    )
  )
  expect_equal(nrow(qmat_valid_res), 35)
  expect_equal(
    nrow(
      qmat_valid_res |>
        dplyr::filter(is.na(empirical_specification))
    ),
    35
  )
})

test_that("qmatrix validation errors for 1 attribute", {
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
      seed = 63277
    )
  )

  err <- rlang::catch_cnd(qmatrix_validation(x = dina_mod))
  expect_match(
    err$message,
    paste0(
      "The Q-matrix validation method can only be applied to assessments ",
      "measuring more than one attribute."
    )
  )
})
