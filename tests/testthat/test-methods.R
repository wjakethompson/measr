test_that("returned predictions have correct dimensions and names", {
  num_att <- ncol(rstn_dina$data$qmatrix) - 1
  prof_labs <- profile_labels(num_att)

  mod_preds <- predict(rstn_dina, summary = FALSE)
  expect_equal(names(mod_preds), c("class_probabilities",
                                   "attribute_probabilities"))
  expect_equal(colnames(mod_preds$class_probabilities),
               c("resp_id", prof_labs$class))
  expect_equal(colnames(mod_preds$attribute_probabilities),
               c("resp_id", paste0("att", seq_len(num_att))))
  expect_equal(nrow(mod_preds$class_probabilities), nrow(dina_data))
  expect_equal(nrow(mod_preds$attribute_probabilities), nrow(dina_data))

  mod_preds <- predict(rstn_dina, summary = TRUE)
  expect_equal(names(mod_preds), c("class_probabilities",
                                   "attribute_probabilities"))
  expect_equal(colnames(mod_preds$class_probabilities),
               c("resp_id", "class", "probability"))
  expect_equal(colnames(mod_preds$attribute_probabilities),
               c("resp_id", "attribute", "probability"))
  expect_equal(nrow(mod_preds$class_probabilities),
               nrow(dina_data) * (2 ^ num_att))
  expect_equal(nrow(mod_preds$attribute_probabilities),
               nrow(dina_data) * num_att)
  expect_true(all(mod_preds$class_probabilities$class %in% prof_labs$class))
  expect_true(all(mod_preds$attribute_probabilities$attribute %in%
                    paste0("att", seq_len(num_att))))

})

test_that("dina probabilities are accurate", {
  dina_preds <- predict(rstn_dina, summary = TRUE)

  # extract works
  expect_equal(rstn_dina$respondent_estimates, list())
  err <- rlang::catch_cnd(measr_extract(rstn_dina, "class_prob"))
  expect_match(err$message,
               "added to a model object before class probabilities")
  err <- rlang::catch_cnd(measr_extract(rstn_dina, "attribute_prob"))
  expect_match(err$message,
               "added to a model object before attribute probabilities")

  rstn_dina <- add_respondent_estimates(rstn_dina)
  expect_equal(rstn_dina$respondent_estimates, dina_preds)

  expect_equal(measr_extract(rstn_dina, "class_prob"),
               dina_preds$class_probabilities %>%
                 dplyr::select("resp_id", "class", "probability") %>%
                 tidyr::pivot_wider(names_from = "class",
                                    values_from = "probability"))
  expect_equal(measr_extract(rstn_dina, "attribute_prob"),
               dina_preds$attribute_prob %>%
                 dplyr::select("resp_id", "attribute", "probability") %>%
                 tidyr::pivot_wider(names_from = "attribute",
                                    values_from = "probability"))

  check_dina_predict <- predict(rstn_dina)
  expect_equal(check_dina_predict, rstn_dina$respondent_estimates)

  prof_labs <- profile_labels(ncol(rstn_dina$data$qmatrix) - 1)

  measr_class <- dina_preds$class_probabilities %>%
    dplyr::select("resp_id", "class", "probability") %>%
    dplyr::mutate(resp_id = as.integer(as.character(.data$resp_id)))

  class_diff <- true_profiles %>%
    tibble::rowid_to_column(var = "resp_id") %>%
    dplyr::mutate(profile = paste0("[", .data$att1, ",", .data$att2, ",",
                                   .data$att3, ",", .data$att4, ",",
                                   .data$att5, "]"),
                  true = 1) %>%
    dplyr::select("resp_id", "profile", "true") %>%
    dplyr::right_join(measr_class, by = c("resp_id", "profile" = "class")) %>%
    dplyr::mutate(true = tidyr::replace_na(.data$true, 0),
                  diff = .data$true - .data$probability)

  expect_lt(abs(mean(class_diff$diff)), .03)
  expect_lt(abs(median(class_diff$diff)), .03)


  measr_attr <- dina_preds$attribute_probabilities %>%
    dplyr::mutate(resp_id = as.integer(as.character(.data$resp_id)))

  attr_diff <- true_profiles %>%
    tibble::rowid_to_column(var = "resp_id") %>%
    tidyr::pivot_longer(cols = -"resp_id", names_to = "attribute",
                        values_to = "true") %>%
    dplyr::left_join(measr_attr, by = c("resp_id", "attribute")) %>%
    dplyr::mutate(diff = .data$true - .data$probability)

  expect_lt(abs(mean(attr_diff$diff)), .03)
  expect_lt(abs(median(attr_diff$diff)), .03)
})
