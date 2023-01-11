test_that("dina model works", {
  # optim model -----
  out <- capture.output(
    suppressMessages(
      dina <- measr_dcm(data = dina_data, missing = NA, qmatrix = q_matrix,
                        resp_id = "resp_id", item_id = "item", type = "dina",
                        method = "optim", seed = 63277)
    )
  )

  expect_s3_class(dina, "measrfit")
  expect_s3_class(dina, "measrdcm")
  expect_equal(names(dina),
               c("data", "prior", "stancode", "method", "algorithm",
                 "backend", "model", "model_fit", "criteria", "reliability",
                 "file", "version"))
  expect_equal(names(dina$data), c("data", "qmatrix"))
  expect_equal(dina$data$data,
               dina_data %>%
                 tidyr::pivot_longer(-resp_id, names_to = "item_id",
                                     values_to = "score") %>%
                 dplyr::mutate(resp_id = factor(resp_id),
                               item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(dina$data$qmatrix,
               q_matrix %>%
                 dplyr::rename(item_id = item) %>%
                 dplyr::mutate(item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(dina$prior, default_dcm_priors(type = "dina"))
  expect_snapshot(dina$stancode, variant = "dina-code")
  expect_equal(dina$method, "optim")
  expect_equal(dina$algorithm, "LBFGS")
  expect_type(dina$model, "list")
  expect_equal(names(dina$model),
               c("par", "value", "return_code", "theta_tilde"))
  expect_type(dina$model_fit, "list")
  expect_type(dina$criteria, "list")
  expect_type(dina$reliability, "list")
  expect_null(dina$file)
  expect_equal(names(dina$version), c("measr", "rstan", "StanHeaders"))

  dina_comp <- tibble::enframe(dina$model$par) %>%
    dplyr::filter(grepl("^Vc|slip|guess", .data$name)) %>%
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) %>%
    dplyr::left_join(true_dinoa, by = c("name" = "param"))

  comp_cor <- cor(dina_comp$value, dina_comp$true)
  comp_dif <- abs(dina_comp$value - dina_comp$true)

  expect_true(comp_cor > 0.85)
  expect_true(max(comp_dif) < 0.2)
})

test_that("dino model works", {
  # optim model -----
  out <- capture.output(
    suppressMessages(
      dino <- measr_dcm(data = dino_data, missing = NA, qmatrix = q_matrix,
                        resp_id = "resp_id", item_id = "item", type = "dino",
                        method = "optim", seed = 63277)
    )
  )

  expect_s3_class(dino, "measrfit")
  expect_s3_class(dino, "measrdcm")
  expect_equal(names(dino),
               c("data", "prior", "stancode", "method", "algorithm",
                 "backend", "model", "model_fit", "criteria", "reliability",
                 "file", "version"))
  expect_equal(names(dino$data), c("data", "qmatrix"))
  expect_equal(dino$data$data,
               dino_data %>%
                 tidyr::pivot_longer(-resp_id, names_to = "item_id",
                                     values_to = "score") %>%
                 dplyr::mutate(resp_id = factor(resp_id),
                               item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(dino$data$qmatrix,
               q_matrix %>%
                 dplyr::rename(item_id = item) %>%
                 dplyr::mutate(item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(dino$prior, default_dcm_priors(type = "dino"))
  expect_snapshot(dino$stancode, variant = "dino-code")
  expect_equal(dino$method, "optim")
  expect_equal(dino$algorithm, "LBFGS")
  expect_type(dino$model, "list")
  expect_equal(names(dino$model),
               c("par", "value", "return_code", "theta_tilde"))
  expect_type(dino$model_fit, "list")
  expect_type(dino$criteria, "list")
  expect_type(dino$reliability, "list")
  expect_null(dino$file)
  expect_equal(names(dino$version), c("measr", "rstan", "StanHeaders"))

  dino_comp <- tibble::enframe(dino$model$par) %>%
    dplyr::filter(grepl("^Vc|slip|guess", .data$name)) %>%
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) %>%
    dplyr::left_join(true_dinoa, by = c("name" = "param"))

  comp_cor <- cor(dino_comp$value, dino_comp$true)
  comp_dif <- abs(dino_comp$value - dino_comp$true)

  expect_true(comp_cor > 0.85)
  expect_true(max(comp_dif) < 0.2)
})

test_that("lcdm model works", {
  # optim model -----
  out <- capture.output(
    suppressMessages(
      lcdm <- measr_dcm(data = ecpe_data, missing = NA, qmatrix = ecpe_qmatrix,
                        resp_id = "resp_id", item_id = "item_id", type = "lcdm",
                        method = "optim", seed = 63277)
    )
  )

  expect_s3_class(lcdm, "measrfit")
  expect_s3_class(lcdm, "measrdcm")
  expect_equal(names(lcdm),
               c("data", "prior", "stancode", "method", "algorithm",
                 "backend", "model", "model_fit", "criteria", "reliability",
                 "file", "version"))
  expect_equal(names(lcdm$data), c("data", "qmatrix"))
  expect_equal(lcdm$data$data,
               ecpe_data %>%
                 tidyr::pivot_longer(-resp_id, names_to = "item_id",
                                     values_to = "score") %>%
                 dplyr::mutate(resp_id = factor(resp_id),
                               item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(lcdm$data$qmatrix,
               ecpe_qmatrix %>%
                 dplyr::mutate(item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(lcdm$prior, default_dcm_priors(type = "lcdm"))
  expect_snapshot(lcdm$stancode, variant = "lcdm-code")
  expect_equal(lcdm$method, "optim")
  expect_equal(lcdm$algorithm, "LBFGS")
  expect_type(lcdm$model, "list")
  expect_equal(names(lcdm$model),
               c("par", "value", "return_code", "theta_tilde"))
  expect_type(lcdm$model_fit, "list")
  expect_type(lcdm$criteria, "list")
  expect_type(lcdm$reliability, "list")
  expect_null(lcdm$file)
  expect_equal(names(lcdm$version), c("measr", "rstan", "StanHeaders"))

  lcdm_comp <- tibble::enframe(lcdm$model$par) %>%
    dplyr::filter(grepl("^Vc|^l[0-9]*_[0-9]*$", .data$name)) %>%
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) %>%
    dplyr::full_join(true_lcdm, by = c("name" = "parameter"))

  comp_cor <- cor(lcdm_comp$value, lcdm_comp$true)
  expect_true(comp_cor > 0.85)
})
