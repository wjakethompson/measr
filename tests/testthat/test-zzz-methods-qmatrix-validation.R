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
  expect_equal(nrow(qmat_valid_res), 28)
  expect_equal(
    nrow(
      qmat_valid_res |>
        dplyr::filter(!validation_flag)
    ),
    28
  )
})
