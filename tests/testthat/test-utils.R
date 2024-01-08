test_that("profiles get created in expected order", {
  expect_identical(create_profiles(1),
                   tibble::tibble(att1 = c(0L, 1L)))

  expect_identical(create_profiles(2),
                   tibble::tibble(att1 = c(0L, 1L, 0L, 1L),
                                  att2 = c(0L, 0L, 1L, 1L)))

  expect_identical(create_profiles(3),
                   tibble::tribble(~att1, ~att2, ~att3,
                                      0L,    0L,    0L,
                                      1L,    0L,    0L,
                                      0L,    1L,    0L,
                                      0L,    0L,    1L,
                                      1L,    1L,    0L,
                                      1L,    0L,    1L,
                                      0L,    1L,    1L,
                                      1L,    1L,    1L))

  expect_identical(create_profiles(4),
                   tibble::tribble(~att1, ~att2, ~att3, ~att4,
                                      0L,    0L,    0L,    0L,
                                      1L,    0L,    0L,    0L,
                                      0L,    1L,    0L,    0L,
                                      0L,    0L,    1L,    0L,
                                      0L,    0L,    0L,    1L,
                                      1L,    1L,    0L,    0L,
                                      1L,    0L,    1L,    0L,
                                      1L,    0L,    0L,    1L,
                                      0L,    1L,    1L,    0L,
                                      0L,    1L,    0L,    1L,
                                      0L,    0L,    1L,    1L,
                                      1L,    1L,    1L,    0L,
                                      1L,    1L,    0L,    1L,
                                      1L,    0L,    1L,    1L,
                                      0L,    1L,    1L,    1L,
                                      1L,    1L,    1L,    1L))
})

test_that("release bullets can be added to release issues", {
  expect_type(release_bullets(), "character")
  expect_equal(length(release_bullets()), 1)
})
