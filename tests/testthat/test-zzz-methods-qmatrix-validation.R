test_that("Q-matrix validation works for ecpe", {
  rstn_dina <- add_respondent_estimates(rstn_dina)
  qmat_valid_res <- qmatrix_validation(x = rstn_dina)

  expect_equal(
    names(qmat_valid_res),
    c(
      "item_id",
      "validation_flag",
      "original_specification",
      "empirical_specification",
      "pvaf"
    )
  )
  expect_equal(nrow(qmat_valid_res), 35)
  expect_equal(
    nrow(
      qmat_valid_res |>
        dplyr::filter(!validation_flag)
    ),
    35
  )

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
    paste0("The Q-matrix validation method can only be applied to assessments ",
           "measuring more than one attribute.")
  )
})
