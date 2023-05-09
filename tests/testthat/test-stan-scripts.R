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
  expect_snapshot(gqs_script(full_data = TRUE))

  prob_code <- as.character(gqs_script()$stancode)
  prob_code <- gsub("\\n\\n", "\\\n", prob_code)
  prob_code <- gsub("\\n  \\n", "\\\n", prob_code)
  prob_code <- gsub("\\} ", "}", prob_code)

  expect_equal(prob_code, stanmodels$gqs_probs@model_code,
               ignore_attr = TRUE)

  ppmc_code <- as.character(gqs_script(full_data = TRUE)$stancode)
  ppmc_code <- gsub("\\n\\n", "\\\n", ppmc_code)
  ppmc_code <- gsub("\\n  \\n", "\\\n", ppmc_code)
  ppmc_code <- gsub("\\} ", "}", ppmc_code)

  expect_equal(ppmc_code, stanmodels$gqs_ppmc@model_code,
               ignore_attr = TRUE)
})

test_that("stan log_lik script works", {
  expect_snapshot(loglik_script())

  loglik_code <- as.character(loglik_script()$stancode)
  loglik_code <- gsub("\\n\\n", "\\\n", loglik_code)

  expect_equal(loglik_code, stanmodels$gqs_loglik@model_code,
               ignore_attr = TRUE)
})

test_that("lcdm script works", {
  expect_snapshot(lcdm_script(ecpe_q))
  expect_snapshot(lcdm_script(mdm_q))
  expect_snapshot(lcdm_script(dtmr_q))

  expect_snapshot(lcdm_script(ecpe_q, strc = "independent"))
})

test_that("crum script works", {
  expect_snapshot(crum_script(ecpe_q))
  expect_snapshot(crum_script(mdm_q))
  expect_snapshot(crum_script(dtmr_q))

  expect_snapshot(crum_script(mdm_q, strc = "independent"))
})

test_that("dina and dino script works", {
  expect_snapshot(dina_script(ecpe_q))
  expect_snapshot(dino_script(mdm_q))
  expect_snapshot(dina_script(dtmr_q))

  expect_snapshot(dino_script(dtmr_q, strc = "independent"))
})
