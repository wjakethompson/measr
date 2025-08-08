test_that("Q-matrix validation works for ecpe", {
  mod_spec <- dcm_specify(
    qmatrix = dcmdata::ecpe_qmatrix,
    identifier = "item_id"
  )

  out <- capture.output(
    suppressMessages(
      rstn_ecpe_lcdm <- dcm_estimate(mod_spec, data = dcmdata::ecpe_data,
                                     identifier = "resp_id",
                                     backend = "rstan",
                                     method = "optim")
    )
  )
  rstn_ecpe_lcdm <- add_respondent_estimates(rstn_ecpe_lcdm)
  qmat_valid_res <- qmatrix_validation(x = rstn_ecpe_lcdm)

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
  expect_equal(nrow(qmat_valid_res), 28)
  expect_equal(
    nrow(
      qmat_valid_res |>
        dplyr::filter(!validation_flag)
    ),
    28
  )
})
