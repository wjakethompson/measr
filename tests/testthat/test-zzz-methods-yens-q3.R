test_that("Yen's Q3 works", {
  q3_dina <- add_respondent_estimates(rstn_dina)
  yens_output <- yens_q3(q3_dina)

  expect_equal(nrow(yens_output), ((35 * (35 + 1)) / 2) - 35)
  expect_equal(ncol(yens_output), 4)
  expect_equal(names(yens_output), c("item_1", "item_2", "resid_corr", "flag"))

  q3max <- yens_q3(q3_dina, summary = "q3max")
  expect_true(is.numeric(q3max))
  expect_true(dplyr::between(q3max, 0, 1))
  expect_equal(q3max, max(abs(yens_output$resid_corr)))

  q3star <- yens_q3(q3_dina, summary = "q3star")
  expect_true(is.numeric(q3star))
  expect_true(dplyr::between(q3star, 0, 1))
  expect_equal(q3star,
               max(abs(yens_output$resid_corr)) -
                 mean(abs(yens_output$resid_corr)))
})

test_that("respondent estimates get added", {
  dino_output <- yens_q3(rstn_dino)

  expect_equal(nrow(dino_output), ((35 * (35 + 1)) / 2) - 35)
  expect_equal(ncol(dino_output), 4)
  expect_equal(names(dino_output), c("item_1", "item_2", "resid_corr", "flag"))
})
