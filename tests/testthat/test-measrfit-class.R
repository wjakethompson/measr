test_that("validation works", {
  expect_identical(validate_measrfit(rstn_dina), rstn_dina)
  expect_identical(validate_measrfit(rstn_dino), rstn_dino)
})

test_that("creation works", {
  expect_identical(
    rstn_dina,
    measrfit(
      data = rstn_dina$data,
      type = rstn_dina$type,
      prior = rstn_dina$prior,
      stancode = rstn_dina$stancode,
      method = rstn_dina$method,
      algorithm = rstn_dina$algorithm,
      backend = rstn_dina$backend,
      model = rstn_dina$model,
      respondent_estimates = rstn_dina$respondent_estimates,
      fit = rstn_dina$fit,
      criteria = rstn_dina$criteria,
      reliability = rstn_dina$reliability,
      file = rstn_dina$file,
      version = rstn_dina$version,
      class = "measrdcm"
    )
  )
})

test_that("coercion works", {
  expect_identical(
    rstn_dino,
    as_measrfit(rstn_dino, class = "measrdcm")
  )
})

test_that("class check works", {
  expect_true(is_measrfit(rstn_dina))
  expect_true(is_measrfit(rstn_dino))
  expect_false(is_measrfit("blue"))
  expect_false(is_measrfit(list()))
})
