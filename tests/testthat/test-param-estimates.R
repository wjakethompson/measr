test_that("dina model works", {
  expect_s3_class(rstn_dina, "measrfit")
  expect_s3_class(rstn_dina, "measrdcm")
  expect_equal(names(rstn_dina),
               c("data", "type", "prior", "stancode", "method", "algorithm",
                 "backend", "model", "respondent_estimates", "fit", "criteria",
                 "reliability", "file", "version"))
  expect_equal(names(rstn_dina$data),
               c("data", "qmatrix", "resp_id", "item_id"))
  expect_equal(rstn_dina$data$data,
               dina_data %>%
                 tidyr::pivot_longer(-resp_id, names_to = "item_id",
                                     values_to = "score") %>%
                 dplyr::mutate(resp_id = factor(resp_id),
                               item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(rstn_dina$data$qmatrix,
               q_matrix %>%
                 dplyr::rename(item_id = item) %>%
                 dplyr::mutate(item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(rstn_dina$type, "dina")
  expect_equal(rstn_dina$prior, default_dcm_priors(type = "dina"))
  expect_snapshot(rstn_dina$stancode, variant = "dina-code")
  expect_equal(rstn_dina$method, "optim")
  expect_equal(rstn_dina$algorithm, "LBFGS")
  expect_type(rstn_dina$model, "list")
  expect_equal(names(rstn_dina$model),
               c("par", "value", "return_code", "theta_tilde"))
  expect_type(rstn_dina$respondent_estimates, "list")
  expect_type(rstn_dina$fit, "list")
  expect_type(rstn_dina$criteria, "list")
  expect_type(rstn_dina$reliability, "list")
  expect_null(rstn_dina$file)
  expect_equal(names(rstn_dina$version),
               c("R", "measr", "rstan", "StanHeaders"))

  dina_comp <- tibble::enframe(rstn_dina$model$par) %>%
    dplyr::filter(grepl("^Vc|slip|guess", .data$name)) %>%
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) %>%
    dplyr::left_join(true_dinoa, by = c("name" = "param"))

  comp_cor <- cor(dina_comp$value, dina_comp$true)
  comp_dif <- abs(dina_comp$value - dina_comp$true)

  expect_gte(comp_cor, 0.85)
  expect_lte(max(comp_dif), 0.2)
})

test_that("dino model works", {
  expect_s3_class(rstn_dino, "measrfit")
  expect_s3_class(rstn_dino, "measrdcm")
  expect_equal(names(rstn_dino),
               c("data", "type", "prior", "stancode", "method", "algorithm",
                 "backend", "model", "respondent_estimates", "fit", "criteria",
                 "reliability", "file", "version"))
  expect_equal(names(rstn_dino$data),
               c("data", "qmatrix", "resp_id", "item_id"))
  expect_equal(rstn_dino$data$data,
               dino_data %>%
                 tidyr::pivot_longer(-resp_id, names_to = "item_id",
                                     values_to = "score") %>%
                 dplyr::mutate(resp_id = factor(resp_id),
                               item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(rstn_dino$data$qmatrix,
               q_matrix %>%
                 dplyr::rename(item_id = item) %>%
                 dplyr::mutate(item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(rstn_dino$type, "dino")
  expect_equal(rstn_dino$prior, default_dcm_priors(type = "dino"))
  expect_snapshot(rstn_dino$stancode, variant = "dino-code")
  expect_equal(rstn_dino$method, "optim")
  expect_equal(rstn_dino$algorithm, "LBFGS")
  expect_type(rstn_dino$model, "list")
  expect_equal(names(rstn_dino$model),
               c("par", "value", "return_code", "theta_tilde"))
  expect_type(rstn_dino$respondent_estimates, "list")
  expect_type(rstn_dino$fit, "list")
  expect_type(rstn_dino$criteria, "list")
  expect_type(rstn_dino$reliability, "list")
  expect_null(rstn_dino$file)
  expect_equal(names(rstn_dino$version),
               c("R", "measr", "rstan", "StanHeaders"))

  dino_comp <- tibble::enframe(rstn_dino$model$par) %>%
    dplyr::filter(grepl("^Vc|slip|guess", .data$name)) %>%
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) %>%
    dplyr::left_join(true_dinoa, by = c("name" = "param"))

  comp_cor <- cor(dino_comp$value, dino_comp$true)
  comp_dif <- abs(dino_comp$value - dino_comp$true)

  expect_gte(comp_cor, 0.85)
  expect_lte(max(comp_dif), 0.2)
})
