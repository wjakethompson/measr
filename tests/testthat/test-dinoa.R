test_that("dina model works", {
  # optim model
  out <- capture.output(
    suppressMessages(
      dina <- measr_dcm(data = dina_data, missing = NA, qmatrix = q_matrix,
                        resp_id = "resp_id", item_id = "item", type = "dina",
                        method = "optim")
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
  expect_snapshot(dina$stancode)
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
  expect_true(max(comp_dif) < 0.15)

  # mcmc model
#   skip_on_cran()
#   dina <- measr_dcm(data = dina_data, missing = NA, qmatrix = q_matrix,
#                     resp_id = "resp_id", item_id = "item", type = "dina",
#                     method = "mcmc", iter = 300, cores = 4, chains = 4,
#                     warmup = 150, refresh = 0)
#
#   dina_comp <- rstan::summary(dina$model)$summary %>%
#     tibble::as_tibble(rownames = "param") %>%
#     dplyr::filter(grepl("^Vc|slip|guess", .data$param)) %>%
#     dplyr::mutate(param = gsub("Vc", "nu", .data$param)) %>%
#     dplyr::left_join(true_dinoa, by = "param")
#
#   comp_cor <- cor(dina_comp$mean, dina_comp$true)
#   comp_dif <- abs(dina_comp$mean - dina_comp$true)
#   expect_true(comp_cor > 0.85)
#   expect_true(max(comp_dif) < 0.15)
#
#   expect_s3_class(dina, "measrfit")
#   expect_s3_class(dina, "measrdcm")
#   expect_equal(names(dina),
#                c("data", "prior", "stancode", "method", "algorithm",
#                  "backend", "model", "model_fit", "criteria", "reliability",
#                  "file", "version"))
})
