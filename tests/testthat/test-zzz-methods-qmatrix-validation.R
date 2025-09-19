test_that("Q-matrix validation works for ecpe", {
  # correctly specified lcdm model with ecpe data ------------------------------
  # should match the results from the gdina package ----------------------------
  qmat_valid_res <- qmatrix_validation(x = rstn_lcdm)
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
  expect_equal(nrow(qmat_valid_res), 28)
  expect_equal(
    nrow(
      qmat_valid_res |>
        dplyr::filter(is.na(empirical_specification))
    ),
    26
  )
  expect_equal(qmat_valid_res |>
                 dplyr::filter(!is.na(empirical_specification)) |>
                 dplyr::pull(.data$item_id),
               c("E9", "E13"))
  expect_equal(qmat_valid_res |>
                 dplyr::filter(.data$item_id %in% c("E9", "E13")) |>
                 dplyr::pull(.data$empirical_specification),
               c("[1, 0, 1]", "[1, 0, 1]"))
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
