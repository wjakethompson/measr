skip_on_cran()

out <- capture.output(
  suppressMessages(
    rstn_ecpe_lcdm <- measr_dcm(
      data = ecpe_data, missing = NA, qmatrix = ecpe_qmatrix,
      resp_id = "resp_id", item_id = "item_id", type = "lcdm",
      method = "optim", backend = "cmdstanr",
      prior = c(prior(uniform(-15, 15), class = "intercept"),
                prior(uniform(0, 15), class = "maineffect"),
                prior(uniform(-15, 15), class = "interaction")))
  )
)

test_that("lcdm model works for ecpe", {
  expect_s3_class(rstn_ecpe_lcdm, "measrfit")
  expect_s3_class(rstn_ecpe_lcdm, "measrdcm")
  expect_equal(names(rstn_ecpe_lcdm),
               c("data", "type", "prior", "stancode", "method", "algorithm",
                 "backend", "model", "model_fit", "criteria", "reliability",
                 "file", "version"))
  expect_equal(names(rstn_ecpe_lcdm$data),
               c("data", "qmatrix", "resp_id", "item_id"))
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
  expect_equal(rstn_ecpe_lcdm$prior,
               c(prior(uniform(-15, 15), class = "intercept"),
                 prior(uniform(0, 15), class = "maineffect"),
                 prior(uniform(-15, 15), class = "interaction")))
  expect_snapshot(rstn_ecpe_lcdm$stancode, variant = "lcdm-ecpe-code")
  expect_equal(rstn_ecpe_lcdm$method, "optim")
  expect_equal(tolower(rstn_ecpe_lcdm$algorithm), "lbfgs")
  expect_type(rstn_ecpe_lcdm$model, "environment")
  # expect_equal(names(rstn_ecpe_lcdm$model),
  #              c("par", "value", "return_code", "theta_tilde"))
  expect_equal(class(rstn_ecpe_lcdm$model), c("CmdStanMLE", "CmdStanFit", "R6"))
  expect_type(rstn_ecpe_lcdm$model_fit, "list")
  expect_type(rstn_ecpe_lcdm$criteria, "list")
  expect_type(rstn_ecpe_lcdm$reliability, "list")
  expect_null(rstn_ecpe_lcdm$file)
  expect_equal(names(rstn_ecpe_lcdm$version),
               c("R", "measr", "rstan", "StanHeaders", "cmdstanr", "cmdstan"))

  expect_equal(rstn_ecpe_lcdm$model$lp(), ecpe_lldcm$logLik, tolerance = 0.01)

  lcdm_comp <- tibble::enframe(rstn_ecpe_lcdm$model$mle()) %>%
    dplyr::filter(grepl("^Vc|^l[0-9]*_[0-9]*$", .data$name)) %>%
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) %>%
    dplyr::full_join(true_lcdm, by = c("name" = "parameter"))

  comp_cor <- cor(lcdm_comp$value, lcdm_comp$true)
  expect_gte(comp_cor, 0.85)
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

  class_diff <- abs(
    round(measr_class, digits = 4) -
    round(ecpe_lldcm$posterior[, c(1, 5, 3, 2, 7, 6, 4, 8)], digits = 4))

  expect_lt(mean(class_diff), .02)
  expect_lt(median(class_diff), .02)


  measr_attr <- ecpe_preds$attribute_probabilities %>%
    dplyr::select("resp_id", "attribute", "mean") %>%
    tidyr::pivot_wider(names_from = "attribute", values_from = "mean") %>%
    dplyr::select(-"resp_id") %>%
    as.matrix() %>%
    unname()

  attr_diff <- abs(round(measr_attr, digits = 4) -
                     round(ecpe_lldcm$eap, digits = 4))

  expect_lt(mean(attr_diff), .02)
  expect_lt(median(attr_diff), .02)
})

test_that("ecpe reliability", {
  ecpe_reli <- reliability(rstn_ecpe_lcdm)

  # list naming
  expect_equal(names(ecpe_reli), c("pattern_reliability", "map_reliability",
                                  "eap_reliability"))
  expect_equal(names(ecpe_reli$pattern_reliability), c("p_a", "p_c"))
  expect_equal(names(ecpe_reli$map_reliability), c("accuracy", "consistency"))
  expect_equal(names(ecpe_reli$map_reliability$accuracy),
               c("attribute", "acc", "lambda_a", "kappa_a", "youden_a",
                 "tetra_a", "tp_a", "tn_a"))
  expect_equal(names(ecpe_reli$map_reliability$consistency),
               c("attribute", "consist", "lambda_c", "kappa_c", "youden_c",
                 "tetra_c", "tp_c", "tn_c", "gammak", "pc_prime"))
  expect_equal(names(ecpe_reli$eap_reliability),
               c("attribute", "rho_pf", "rho_bs", "rho_i", "rho_tb"))

  # list rows
  expect_equal(ecpe_reli$map_reliability$accuracy$attribute,
               colnames(ecpe_qmatrix)[-1])
  expect_equal(ecpe_reli$map_reliability$consistency$attribute,
               colnames(ecpe_qmatrix)[-1])
  expect_equal(ecpe_reli$eap_reliability$attribute,
               colnames(ecpe_qmatrix)[-1])

  # reliability values
  patt_diff <- abs(round(unname(ecpe_reli$pattern_reliability), digits = 4) -
                     round(unname(ecpe_lldcm_reli[[1]]), digits = 4))
  expect_lt(mean(patt_diff), .01)
  expect_lt(median(patt_diff), .01)

  map_acc_diff <- abs(
    round(as.matrix(ecpe_reli$map_reliability$accuracy[, -1]), digits = 4) -
      round(as.matrix(ecpe_lldcm_reli[[2]][1:7]), digits = 4)
  )
  expect_lt(mean(map_acc_diff), .02)
  expect_lt(median(map_acc_diff), .01)

  map_con_diff <- abs(
    round(as.matrix(ecpe_reli$map_reliability$consistency[, -1]), digits = 4) -
      round(as.matrix(ecpe_lldcm_reli[[2]][8:16]), digits = 4)
  )
  expect_lt(mean(map_con_diff), .01)
  expect_lt(median(map_con_diff), .01)

  eap_diff <- abs(
    round(as.matrix(ecpe_reli$eap_reliability[, -1]), digits = 4) -
      round(as.matrix(ecpe_lldcm_reli[[3]]), digits = 4)
  )
  expect_lt(mean(eap_diff), .01)
  expect_lt(median(eap_diff), .01)
})
