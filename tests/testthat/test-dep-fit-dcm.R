test_that("measr_dcm is deprecated", {
  err <- rlang::catch_cnd(
    measr_dcm(
      data = dcmdata::mdm_data,
      missing = NA,
      qmatrix = dcmdata::mdm_data,
      resp_id = "respondent",
      item_id = "item",
      type = "lcdm",
      attribute_structure = "unconstrained",
      method = "optim"
    )
  )
  expect_equal(err$function_nm, "measr_dcm")
  expect_match(err$message, "is deprecated")
  expect_equal(err$stage, "deprecated")
  expect_equal(err$package, "lifecycle")
})

test_that("measr_dcm works", {
  skip_on_cran()

  suppressWarnings(
    mod1 <- measr_dcm(
      data = dcmdata::mdm_data,
      missing = NA,
      qmatrix = dcmdata::mdm_qmatrix,
      resp_id = "respondent",
      item_id = "item",
      type = "lcdm",
      attribute_structure = "unconstrained",
      method = "optim"
    )
  )

  suppressWarnings(
    mod2 <- measr_dcm(
      data = dcmdata::mdm_data,
      missing = NA,
      qmatrix = dcmdata::mdm_qmatrix,
      resp_id = "respondent",
      item_id = "item",
      type = "dina",
      attribute_structure = "independent",
      method = "optim"
    )
  )

  suppressWarnings(
    mod3 <- measr_dcm(
      data = dcmdata::mdm_data,
      missing = NA,
      qmatrix = dcmdata::mdm_qmatrix,
      resp_id = "respondent",
      item_id = "item",
      type = "dino",
      attribute_structure = "unconstrained",
      method = "optim"
    )
  )

  suppressWarnings(
    mod4 <- measr_dcm(
      data = dcmdata::mdm_data,
      missing = NA,
      qmatrix = dcmdata::mdm_qmatrix,
      resp_id = "respondent",
      item_id = "item",
      type = "crum",
      attribute_structure = "independent",
      method = "optim"
    )
  )

  expect_s7_class(mod1, measrfit)
  expect_s7_class(mod1, measrdcm)
  expect_s7_class(mod2, measrfit)
  expect_s7_class(mod2, measrdcm)
  expect_s7_class(mod3, measrfit)
  expect_s7_class(mod3, measrdcm)
  expect_s7_class(mod4, measrfit)
  expect_s7_class(mod4, measrdcm)
})
