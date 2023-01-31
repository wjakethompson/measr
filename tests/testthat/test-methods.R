test_that("returned predictions have correct dimensions and names", {
  num_att <- ncol(rstn_mdm_lcdm$data$qmatrix) - 1

  mod_preds <- predict(rstn_mdm_lcdm, summary = FALSE)
  expect_equal(names(mod_preds), c("class_probabilities",
                                   "attribute_probabilities"))
  expect_equal(colnames(mod_preds$class_probabilities),
               c(".chain", ".iteration", ".draw", "respondent", "[0]", "[1]"))
  expect_equal(colnames(mod_preds$attribute_probabilities),
               c(".chain", ".iteration", ".draw", "respondent",
                 "multiplication"))
  expect_equal(nrow(mod_preds$class_probabilities), nrow(mdm_data))
  expect_equal(nrow(mod_preds$attribute_probabilities), nrow(mdm_data))

  mod_preds <- predict(rstn_mdm_lcdm, summary = TRUE)
  expect_equal(names(mod_preds), c("class_probabilities",
                                   "attribute_probabilities"))
  expect_equal(colnames(mod_preds$class_probabilities),
               c("respondent", "class", "mean", "2.5%", "97.5%"))
  expect_equal(colnames(mod_preds$attribute_probabilities),
               c("respondent", "attribute", "mean", "2.5%", "97.5%"))
  expect_equal(nrow(mod_preds$class_probabilities),
               nrow(mdm_data) * (2 ^ num_att))
  expect_equal(nrow(mod_preds$attribute_probabilities),
               nrow(mdm_data) * num_att)
})

test_that("mdm probabilities are accurate", {
  mdm_preds <- predict(rstn_mdm_lcdm, summary = TRUE)

  measr_class <- mdm_preds$class_probabilities %>%
    dplyr::select("respondent", "class", "mean") %>%
    tidyr::pivot_wider(names_from = "class", values_from = "mean") %>%
    dplyr::select(-"respondent") %>%
    as.matrix() %>%
    unname()
  expect_equal(measr_class, mdm_lldcm$posterior, tolerance = 0.01)

  measr_attr <- mdm_preds$attribute_probabilities %>%
    dplyr::select("mean") %>%
    as.matrix() %>%
    unname()
  expect_equal(measr_attr, mdm_lldcm$eap, tolerance = 0.01)
})

test_that("ecpe probabilities are accurate", {
  ecpe_preds <- predict(rstn_ecpe_lcdm, newdata = ecpe_data,
                        resp_id = "resp_id", summary = TRUE)

  measr_class <- ecpe_preds$class_probabilities %>%
    dplyr::select("resp_id", "class", "mean") %>%
    tidyr::pivot_wider(names_from = "class", values_from = "mean") %>%
    dplyr::select(-"resp_id") %>%
    as.matrix() %>%
    unname()
  expect_equal(measr_class, ecpe_lldcm$posterior[, c(1, 5, 3, 2, 7, 6, 4, 8)],
               tolerance = 0.1)

  measr_attr <- ecpe_preds$attribute_probabilities %>%
    dplyr::select("resp_id", "attribute", "mean") %>%
    tidyr::pivot_wider(names_from = "attribute", values_from = "mean") %>%
    dplyr::select(-"resp_id") %>%
    as.matrix() %>%
    unname()
  expect_equal(measr_attr, ecpe_lldcm$eap, tolerance = 0.1)
})
