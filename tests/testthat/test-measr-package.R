test_that("release bullets can be added to release issues", {
  expect_type(release_bullets(), "character")
  expect_equal(length(release_bullets()), 1)
})
