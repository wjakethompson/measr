test_that("stan generated quantities script works", {
  expect_snapshot(gqs_script(), variant = "gqs-code")
})
