test_that("difficulty works", {
  dina_diffic <- difficulty(rstn_dina)

  expect_equal(length(dina_diffic), 1L)
  expect_equal(names(dina_diffic),
               c("weighted_pvalue"))

  expect_equal(names(dina_diffic$weighted_pvalue),
               c("item", "weighted_pval"))
  expect_equal(nrow(dina_diffic$weighted_pvalue), 35L)
})
