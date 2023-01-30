test_that("dina model works", {
  expect_s3_class(rstn_dina, "measrfit")
  expect_s3_class(rstn_dina, "measrdcm")
  expect_equal(names(rstn_dina),
               c("data", "type", "prior", "stancode", "method", "algorithm",
                 "backend", "model", "model_fit", "criteria", "reliability",
                 "file", "version"))
  expect_equal(names(rstn_dina$data), c("data", "qmatrix", "resp_id", "item_id"))
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
  expect_type(rstn_dina$model_fit, "list")
  expect_type(rstn_dina$criteria, "list")
  expect_type(rstn_dina$reliability, "list")
  expect_null(rstn_dina$file)
  expect_equal(names(rstn_dina$version), c("measr", "rstan", "StanHeaders"))

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
                 "backend", "model", "model_fit", "criteria", "reliability",
                 "file", "version"))
  expect_equal(names(rstn_dino$data), c("data", "qmatrix", "resp_id", "item_id"))
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
  expect_type(rstn_dino$model_fit, "list")
  expect_type(rstn_dino$criteria, "list")
  expect_type(rstn_dino$reliability, "list")
  expect_null(rstn_dino$file)
  expect_equal(names(rstn_dino$version), c("measr", "rstan", "StanHeaders"))

  dino_comp <- tibble::enframe(rstn_dino$model$par) %>%
    dplyr::filter(grepl("^Vc|slip|guess", .data$name)) %>%
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) %>%
    dplyr::left_join(true_dinoa, by = c("name" = "param"))

  comp_cor <- cor(dino_comp$value, dino_comp$true)
  comp_dif <- abs(dino_comp$value - dino_comp$true)

  expect_gte(comp_cor, 0.85)
  expect_lte(max(comp_dif), 0.2)
})

test_that("lcdm model works for ecpe", {
  expect_s3_class(rstn_ecpe_lcdm, "measrfit")
  expect_s3_class(rstn_ecpe_lcdm, "measrdcm")
  expect_equal(names(rstn_ecpe_lcdm),
               c("data", "type", "prior", "stancode", "method", "algorithm",
                 "backend", "model", "model_fit", "criteria", "reliability",
                 "file", "version"))
  expect_equal(names(rstn_ecpe_lcdm$data), c("data", "qmatrix", "resp_id", "item_id"))
  expect_equal(rstn_ecpe_lcdm$data$data,
               ecpe_data %>%
                 tidyr::pivot_longer(-resp_id, names_to = "item_id",
                                     values_to = "score") %>%
                 dplyr::mutate(resp_id = factor(resp_id),
                               item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(rstn_ecpe_lcdm$data$qmatrix,
               ecpe_qmatrix %>%
                 dplyr::mutate(item_id = factor(item_id,
                                                levels = unique(item_id))))
  expect_equal(rstn_ecpe_lcdm$type, "lcdm")
  expect_equal(rstn_ecpe_lcdm$prior, default_dcm_priors(type = "lcdm"))
  expect_snapshot(rstn_ecpe_lcdm$stancode, variant = "lcdm-ecpe-code")
  expect_equal(rstn_ecpe_lcdm$method, "optim")
  expect_equal(rstn_ecpe_lcdm$algorithm, "LBFGS")
  expect_type(rstn_ecpe_lcdm$model, "list")
  expect_equal(names(rstn_ecpe_lcdm$model),
               c("par", "value", "return_code", "theta_tilde"))
  expect_type(rstn_ecpe_lcdm$model_fit, "list")
  expect_type(rstn_ecpe_lcdm$criteria, "list")
  expect_type(rstn_ecpe_lcdm$reliability, "list")
  expect_null(rstn_ecpe_lcdm$file)
  expect_equal(names(rstn_ecpe_lcdm$version), c("measr", "rstan", "StanHeaders"))

  expect_equal(rstn_ecpe_lcdm$model$value, ecpe_lldcm$logLik, tolerance = 0.01)

  lcdm_comp <- tibble::enframe(rstn_ecpe_lcdm$model$par) %>%
    dplyr::filter(grepl("^Vc|^l[0-9]*_[0-9]*$", .data$name)) %>%
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) %>%
    dplyr::full_join(true_lcdm, by = c("name" = "parameter"))

  comp_cor <- cor(lcdm_comp$value, lcdm_comp$true)
  expect_gte(comp_cor, 0.85)
})

test_that("lcdm model works for mdm", {
  resp_names <- dplyr::pull(mdm_data, .data$respondent)

  expect_s3_class(rstn_mdm_lcdm, "measrfit")
  expect_s3_class(rstn_mdm_lcdm, "measrdcm")
  expect_equal(names(rstn_mdm_lcdm),
               c("data", "type", "prior", "stancode", "method", "algorithm",
                 "backend", "model", "model_fit", "criteria", "reliability",
                 "file", "version"))
  expect_equal(names(rstn_mdm_lcdm$data), c("data", "qmatrix", "resp_id", "item_id"))
  expect_equal(rstn_mdm_lcdm$data$data,
               mdm_data %>%
                 tidyr::pivot_longer(-respondent, names_to = "item_id",
                                     values_to = "score") %>%
                 dplyr::mutate(resp_id = factor(respondent, levels = resp_names),
                               item_id = factor(item_id,
                                                levels = unique(item_id))) %>%
                 dplyr::select("resp_id", "item_id", "score"))
  expect_equal(rstn_mdm_lcdm$data$qmatrix,
               mdm_qmatrix %>%
                 dplyr::mutate(item_id = factor(item,
                                                levels = unique(item))) %>%
                 dplyr::select("item_id", "multiplication"))
  expect_equal(rstn_mdm_lcdm$type, "lcdm")
  expect_equal(rstn_mdm_lcdm$prior,
               c(prior(uniform(-15, 15), class = "intercept"),
                 prior(uniform(0, 15), class = "maineffect"),
                 prior(uniform(-15, 15), class = "interaction")))
  expect_snapshot(rstn_mdm_lcdm$stancode, variant = "lcdm-mdm-code")
  expect_equal(rstn_mdm_lcdm$method, "optim")
  expect_equal(rstn_mdm_lcdm$algorithm, "LBFGS")
  expect_type(rstn_mdm_lcdm$model, "list")
  expect_equal(names(rstn_mdm_lcdm$model),
               c("par", "value", "return_code", "theta_tilde"))
  expect_type(rstn_mdm_lcdm$model_fit, "list")
  expect_type(rstn_mdm_lcdm$criteria, "list")
  expect_type(rstn_mdm_lcdm$reliability, "list")
  expect_null(rstn_mdm_lcdm$file)
  expect_equal(names(rstn_mdm_lcdm$version), c("measr", "rstan", "StanHeaders"))

  expect_equal(rstn_mdm_lcdm$model$value, mdm_lldcm$logLik, tolerance = 0.001)

  mdm_true <- dplyr::bind_rows(
    mdm_lldcm$pxi %>%
      rlang::set_names(c("nu[1]", "nu[2]")) %>%
      tibble::enframe(name = "name", value = "true"),
    lapply(mdm_lldcm$beta, tibble::enframe) %>%
      dplyr::bind_rows(.id = "item") %>%
      dplyr::mutate(level = dplyr::case_when(grepl("a1", .data$name) ~ "11",
                                             TRUE ~ "0"),
                    item = gsub("Item", "", .data$item),
                    param = paste0("l", item, "_", level)) %>%
      dplyr::select(name = "param", true = "value")
  )

  lcdm_comp <- tibble::enframe(rstn_mdm_lcdm$model$par) %>%
    dplyr::filter(grepl("^Vc|^l[0-9]*_[0-9]*$", .data$name)) %>%
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) %>%
    dplyr::full_join(mdm_true, by = c("name"))

  comp_cor <- cor(lcdm_comp$value, lcdm_comp$true)
  expect_gte(comp_cor, 0.85)
})
