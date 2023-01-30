ecpe_q <- ecpe_qmatrix %>%
  dplyr::select(-"item_id") %>%
  rlang::set_names(nm = paste0("att", 1:(ncol(ecpe_qmatrix) - 1)))
mdm_q <- mdm_qmatrix %>%
  dplyr::select(-"item") %>%
  rlang::set_names(nm = paste0("att", 1:(ncol(mdm_qmatrix) - 1)))
dtmr_q <- tibble::tibble(
  att1 = c(1L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L,
           0L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L),
  att2 = c(0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
           1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L),
  att3 = c(0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L,
           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
  att4 = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L,
           1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
)

test_that("stan generated quantities script works", {
  expect_snapshot(gqs_script())
})

test_that("lcdm script works", {
  expect_snapshot(lcdm_script(ecpe_q))
  expect_snapshot(lcdm_script(mdm_q))
  expect_snapshot(lcdm_script(dtmr_q))
})

test_that("dina and dino script works", {
  expect_snapshot(dina_script(ecpe_q))
  expect_snapshot(dino_script(mdm_q))
  expect_snapshot(dina_script(dtmr_q))
})
