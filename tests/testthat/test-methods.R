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

  # extract works
  expect_equal(measr_extract(rstn_mdm_lcdm, "class_prob"),
               mdm_preds$class_probabilities %>%
                 dplyr::select("respondent", "class", "mean") %>%
                 tidyr::pivot_wider(names_from = "class", values_from = "mean"))
  expect_equal(measr_extract(rstn_mdm_lcdm, "attribute_prob"),
               mdm_preds$attribute_prob %>%
                 dplyr::select("respondent", "attribute", "mean") %>%
                 tidyr::pivot_wider(names_from = "attribute",
                                    values_from = "mean"))

  measr_class <- mdm_preds$class_probabilities %>%
    dplyr::select("respondent", "class", "mean") %>%
    tidyr::pivot_wider(names_from = "class", values_from = "mean") %>%
    dplyr::select(-"respondent") %>%
    as.matrix() %>%
    unname()

  class_diff <- abs(round(measr_class, digits = 4) -
                      round(mdm_lldcm$posterior, digits = 4))

  expect_lt(mean(class_diff), .02)
  expect_lt(median(class_diff), .02)


  measr_attr <- mdm_preds$attribute_probabilities %>%
    dplyr::select("mean") %>%
    as.matrix() %>%
    unname()

  attr_diff <- abs(round(measr_attr, digits = 4) -
                     round(mdm_lldcm$eap, digits = 4))

  expect_lt(mean(attr_diff), .02)
  expect_lt(median(attr_diff), .02)
})
