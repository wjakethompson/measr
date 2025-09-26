test_that("calc_sigma works - all attributes", {
  # example calculation from de la Torre & Chiu (2016)
  att_names <- c("att1", "att2", "att3", "att4")
  spec <- tibble::tibble(
    att = c("att1", "att2", "att3", "att4"),
    meas = c(1, 1, 1, 1)
  ) |>
    tidyr::pivot_wider(names_from = "att", values_from = "meas")
  strc <- tibble::tibble(
    class = c(
      "0,0,0,0",
      "1,0,0,0",
      "0,1,0,0",
      "0,0,1,0",
      "1,1,0,0",
      "1,0,1,0",
      "0,1,1,0",
      "1,1,1,0",
      "0,0,0,1",
      "1,0,0,1",
      "0,1,0,1",
      "0,0,1,1",
      "1,1,0,1",
      "1,0,1,1",
      "0,1,1,1",
      "1,1,1,1"
    ),
    estimate = c(
      .053,
      .076,
      .039,
      .057,
      .069,
      .047,
      .068,
      .078,
      .037,
      .081,
      .073,
      .055,
      .056,
      .083,
      .069,
      .059
    )
  )
  pi <- tibble::tibble(
    item_id = rep(1, 16),
    profile_id = 1:16,
    prob = c(rep(.225, 7), .725, rep(.225, 7), .725)
  )

  test_sigma <- calc_sigma(
    att_names = att_names,
    q = spec,
    strc_param = strc,
    pi_mat = pi,
    ii = 1
  )

  expect_equal(typeof(test_sigma), "double")
  expect_equal(length(test_sigma), 1)
  expect_equal(test_sigma, .03, tolerance = .015)
})

test_that("calc_sigma works - attribute subset", {
  # example calculation from de la Torre & Chiu (2016)
  att_names <- c("att1", "att2", "att3", "att4")
  spec <- tibble::tibble(
    att = c("att1", "att2", "att3"),
    meas = c(1, 1, 1)
  ) |>
    tidyr::pivot_wider(names_from = "att", values_from = "meas")
  strc <- tibble::tibble(
    class = c(
      "0,0,0,0",
      "1,0,0,0",
      "0,1,0,0",
      "0,0,1,0",
      "1,1,0,0",
      "1,0,1,0",
      "0,1,1,0",
      "1,1,1,0",
      "0,0,0,1",
      "1,0,0,1",
      "0,1,0,1",
      "0,0,1,1",
      "1,1,0,1",
      "1,0,1,1",
      "0,1,1,1",
      "1,1,1,1"
    ),
    estimate = c(
      .053,
      .076,
      .039,
      .057,
      .069,
      .047,
      .068,
      .078,
      .037,
      .081,
      .073,
      .055,
      .056,
      .083,
      .069,
      .059
    )
  )
  pi <- tibble::tibble(
    item_id = rep(1, 16),
    profile_id = 1:16,
    prob = c(rep(.225, 7), .725, rep(.225, 7), .725)
  )

  test_sigma <- calc_sigma(
    att_names,
    q = spec,
    strc_param = strc,
    pi_mat = pi,
    ii = 1
  )

  expect_equal(typeof(test_sigma), "double")
  expect_equal(length(test_sigma), 1)
  expect_equal(test_sigma, 0.02955775, tolerance = .015)

  spec <- tibble::tibble(
    att = c("att2", "att3", "att4"),
    meas = c(1, 1, 1)
  ) |>
    tidyr::pivot_wider(names_from = "att", values_from = "meas")

  test_sigma <- calc_sigma(
    att_names,
    q = spec,
    strc_param = strc,
    pi_mat = pi,
    ii = 1
  )

  expect_equal(typeof(test_sigma), "double")
  expect_equal(length(test_sigma), 1)
  expect_equal(test_sigma, 0.0125393, tolerance = .015)

  spec <- tibble::tibble(
    att = c("att2", "att4"),
    meas = c(1, 1)
  ) |>
    tidyr::pivot_wider(names_from = "att", values_from = "meas")

  test_sigma <- calc_sigma(
    att_names,
    q = spec,
    strc_param = strc,
    pi_mat = pi,
    ii = 1
  )

  expect_equal(typeof(test_sigma), "double")
  expect_equal(length(test_sigma), 1)
  expect_equal(test_sigma, .004564, tolerance = .015)
})
