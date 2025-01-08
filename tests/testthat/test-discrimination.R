test_that("discrimination works", {
  dina_discrim <- cdi(rstn_dina)

  expect_equal(length(dina_discrim), 2L)
  expect_equal(names(dina_discrim),
               c("item_discrimination", "test_discrimination"))

  expect_equal(names(dina_discrim$item_discrimination),
               c("item", "overall", paste0("att", 1:5)))
  expect_equal(nrow(dina_discrim$item_discrimination), 35L)

  expect_equal(names(dina_discrim$test_discrimination),
               c("overall", paste0("att", 1:5)))
  expect_equal(nrow(dina_discrim$test_discrimination), 1L)
  expect_equal(
    dina_discrim$test_discrimination |>
      tidyr::pivot_longer(dplyr::everything()) |>
      tibble::deframe(),
    colSums(dplyr::select(dina_discrim$item_discrimination, -"item"))
  )

  # check different weighting scheme -----
  dina_no_weight <- cdi(rstn_dina, weight_prevalence = FALSE)

  expect_equal(length(dina_no_weight), 2L)
  expect_equal(names(dina_no_weight),
               c("item_discrimination", "test_discrimination"))
  expect_false(identical(dina_discrim, dina_no_weight))

  expect_equal(names(dina_no_weight$item_discrimination),
               c("item", "overall", paste0("att", 1:5)))
  expect_equal(nrow(dina_no_weight$item_discrimination), 35L)
  expect_false(identical(dina_discrim$item_discrimination,
                         dina_no_weight$item_discrimination))

  expect_equal(names(dina_no_weight$test_discrimination),
               c("overall", paste0("att", 1:5)))
  expect_equal(nrow(dina_no_weight$test_discrimination), 1L)
  expect_equal(
    dina_no_weight$test_discrimination |>
      tidyr::pivot_longer(dplyr::everything()) |>
      tibble::deframe(),
    colSums(dplyr::select(dina_no_weight$item_discrimination, -"item"))
  )
  expect_false(identical(dina_discrim$test_discrimination,
                         dina_no_weight$test_discrimination))
})
