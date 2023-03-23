test_that("ECPE data follows expected structure", {
  # ecpe data
  expect_identical(ncol(ecpe_data), 28L + 1L)
  expect_identical(nrow(ecpe_data), 2922L)
  expect_identical(colnames(ecpe_data),
                   c("resp_id", paste0("E", 1:28)))
  expect_identical(ecpe_data$resp_id, 1:2922)

  for (i in 2:ncol(ecpe_data)) {
    expect_true(all(ecpe_data[[i]] %in% c(0L, 1L)))
  }

  # ecpe qmatrix
  expect_identical(ncol(ecpe_qmatrix), 3L + 1L)
  expect_identical(nrow(ecpe_qmatrix), 28L)
  expect_identical(nrow(ecpe_qmatrix), ncol(ecpe_data) - 1L)
  expect_identical(colnames(ecpe_qmatrix),
                   c("item_id", "morphosyntactic", "cohesive", "lexical"))
  expect_identical(ecpe_qmatrix$item_id, paste0("E", 1:28))
  expect_identical(ecpe_qmatrix$item_id, colnames(ecpe_data)[2:29])

  for (i in 2:ncol(ecpe_qmatrix)) {
    expect_true(all(ecpe_data[[i]] %in% c(0L, 1L)))
  }
})
