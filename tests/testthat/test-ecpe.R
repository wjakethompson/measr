if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  skip("No MCMC on CRAN")
} else {
  ecpe_spec <- dcm_specify(
    qmatrix = dcmdata::ecpe_qmatrix,
    identifier = "item_id",
    measurement_model = lcdm(),
    structural_model = unconstrained(),
    priors = c(
      prior(uniform(-15, 15), type = "intercept"),
      prior(uniform(0, 15), type = "maineffect"),
      prior(uniform(-15, 15), type = "interaction")
    )
  )

  out <- capture.output(
    suppressMessages(
      cmds_ecpe_lcdm <- dcm_estimate(
        ecpe_spec,
        data = dcmdata::ecpe_data,
        identifier = "resp_id",
        missing = NA,
        method = "optim",
        backend = "cmdstanr"
      )
    )
  )
}

test_that("lcdm model works for ecpe", {
  skip_on_cran()

  expect_s7_class(cmds_ecpe_lcdm, measrfit)
  expect_s7_class(cmds_ecpe_lcdm, measrdcm)
  expect_identical(cmds_ecpe_lcdm@model_spec@qmatrix, ecpe_spec@qmatrix)
  expect_identical(
    cmds_ecpe_lcdm@model_spec@qmatrix_meta$attribute_names,
    ecpe_spec@qmatrix_meta$attribute_names
  )
  expect_identical(
    cmds_ecpe_lcdm@model_spec@qmatrix_meta$item_identifier,
    "item_id"
  )
  expect_identical(
    cmds_ecpe_lcdm@model_spec@qmatrix_meta$item_names,
    rlang::set_names(1:28, dcmdata::ecpe_qmatrix$item_id)
  )
  expect_identical(
    cmds_ecpe_lcdm@model_spec@measurement_model,
    ecpe_spec@measurement_model
  )
  expect_identical(
    cmds_ecpe_lcdm@model_spec@structural_model,
    ecpe_spec@structural_model
  )
  expect_identical(cmds_ecpe_lcdm@model_spec@priors, ecpe_spec@priors)

  expect_identical(
    cmds_ecpe_lcdm@data$item_identifier,
    cmds_ecpe_lcdm@model_spec@qmatrix_meta$item_identifier
  )
  expect_identical(
    cmds_ecpe_lcdm@data$item_names,
    cmds_ecpe_lcdm@model_spec@qmatrix_meta$item_names
  )

  expect_s3_class(cmds_ecpe_lcdm@stancode, "glue")
  expect_s7_class(cmds_ecpe_lcdm@method, optim)
  expect_s7_class(cmds_ecpe_lcdm@backend, cmdstanr)
  expect_type(cmds_ecpe_lcdm@model, "environment")
  expect_equal(class(cmds_ecpe_lcdm@model), c("CmdStanMLE", "CmdStanFit", "R6"))
  expect_true(
    is.list(cmds_ecpe_lcdm@respondent_estimates) &&
      rlang::is_empty(cmds_ecpe_lcdm@respondent_estimates)
  )
  expect_true(
    is.list(cmds_ecpe_lcdm@fit) &&
      rlang::is_empty(cmds_ecpe_lcdm@fit)
  )
  expect_true(
    is.list(cmds_ecpe_lcdm@criteria) &&
      rlang::is_empty(cmds_ecpe_lcdm@criteria)
  )
  expect_true(
    is.list(cmds_ecpe_lcdm@reliability) &&
      rlang::is_empty(cmds_ecpe_lcdm@reliability)
  )
  expect_true(
    is.character(cmds_ecpe_lcdm@file) &&
      rlang::is_empty(cmds_ecpe_lcdm@file)
  )
  expect_identical(
    names(cmds_ecpe_lcdm@version),
    c("R", "R-measr", "R-cmdstanr", "CmdStan")
  )

  expect_equal(loglik(cmds_ecpe_lcdm), ecpe_lldcm$logLik, tolerance = 0.01)

  lcdm_comp <- get_draws(cmds_ecpe_lcdm) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::matches("^Vc|^l[0-9]*_[0-9]*$")) |>
    tidyr::pivot_longer(cols = everything()) |>
    dplyr::mutate(name = gsub("Vc", "nu", .data$name)) |>
    dplyr::full_join(true_lcdm, by = c("name" = "parameter"))

  comp_cor <- cor(lcdm_comp$value, lcdm_comp$true)
  expect_gte(comp_cor, 0.85)
})

test_that("extract ecpe", {
  skip_on_cran()

  lcdm_param <- measr_extract(cmds_ecpe_lcdm, "item_param")
  all_param <- get_parameters(ecpe_spec) |>
    dplyr::filter(type != "structural")

  expect_equal(nrow(lcdm_param), nrow(all_param))
  expect_equal(
    colnames(lcdm_param),
    c("item_id", "type", "attributes", "coefficient", "estimate")
  )
  expect_equal(lcdm_param$item_id, all_param$item_id)
  expect_equal(lcdm_param$type, all_param$type)
  expect_equal(lcdm_param$attributes, all_param$attributes)
  expect_equal(lcdm_param$coefficient, all_param$coefficient)
  expect_s3_class(lcdm_param$estimate, "rvar")
  expect_true(all(!is.na(lcdm_param$estimate)))

  lcdm_param <- measr_extract(cmds_ecpe_lcdm, "strc_param")
  expect_equal(nrow(lcdm_param), 8)
  expect_equal(lcdm_param$class, dplyr::pull(profile_labels(3), "class"))
  expect_true(is.double(lcdm_param$estimate))
  expect_true(all(!is.na(lcdm_param$estimate)))

  lcdm_param <- measr_extract(cmds_ecpe_lcdm, "prior")
  expect_equal(
    lcdm_param,
    c(
      prior(uniform(-15, 15), type = "intercept"),
      prior(uniform(0, 15), type = "maineffect"),
      prior(uniform(-15, 15), type = "interaction"),
      prior(
        dirichlet(rep_vector(1, C)),
        type = "structural",
        coefficient = "Vc"
      )
    )
  )

  lcdm_param <- measr_extract(cmds_ecpe_lcdm, "classes")
  expect_equal(
    colnames(lcdm_param),
    c("class", "morphosyntactic", "cohesive", "lexical")
  )
  expect_equal(lcdm_param$class, dplyr::pull(profile_labels(3), "class"))
  exp_label <- lcdm_param |>
    dplyr::mutate(
      new_label = paste0("[", morphosyntactic, ",", cohesive, ",", lexical, "]")
    ) |>
    dplyr::pull("new_label")
  expect_equal(lcdm_param$class, exp_label)
})

test_that("ecpe probabilities are accurate", {
  skip_on_cran()

  ecpe_preds <- score(
    cmds_ecpe_lcdm,
    newdata = dcmdata::ecpe_data,
    identifier = "resp_id"
  )

  # dimensions are correct -----
  expect_equal(
    names(ecpe_preds),
    c("class_probabilities", "attribute_probabilities")
  )
  expect_equal(
    colnames(ecpe_preds$class_probabilities),
    c("resp_id", "class", "probability")
  )
  expect_equal(
    colnames(ecpe_preds$attribute_probabilities),
    c("resp_id", "attribute", "probability")
  )
  expect_equal(
    nrow(ecpe_preds$class_probabilities),
    nrow(dcmdata::ecpe_data) * (2^3)
  )
  expect_equal(
    nrow(ecpe_preds$attribute_probabilities),
    nrow(dcmdata::ecpe_data) * 3
  )

  # extract works -----
  expect_equal(cmds_ecpe_lcdm@respondent_estimates, list())
  err <- rlang::catch_cnd(measr_extract(cmds_ecpe_lcdm, "class_prob"))
  expect_match(
    err$message,
    "added to a model object before\\nclass probabilities"
  )
  err <- rlang::catch_cnd(measr_extract(cmds_ecpe_lcdm, "attribute_prob"))
  expect_match(
    err$message,
    "added to a model object before\\nattribute probabilities"
  )

  cmds_ecpe_lcdm <- add_respondent_estimates(cmds_ecpe_lcdm)
  expect_equal(cmds_ecpe_lcdm@respondent_estimates, ecpe_preds)
  expect_equal(
    measr_extract(cmds_ecpe_lcdm, "class_prob"),
    ecpe_preds$class_probabilities |>
      dplyr::select("resp_id", "class", "probability") |>
      tidyr::pivot_wider(names_from = "class", values_from = "probability")
  )
  expect_equal(
    measr_extract(cmds_ecpe_lcdm, "attribute_prob"),
    ecpe_preds$attribute_prob |>
      dplyr::select("resp_id", "attribute", "probability") |>
      tidyr::pivot_wider(names_from = "attribute", values_from = "probability")
  )

  check_preds <- score(cmds_ecpe_lcdm)
  expect_equal(check_preds, cmds_ecpe_lcdm@respondent_estimates)

  measr_class <- ecpe_preds$class_probabilities |>
    dplyr::select("resp_id", "class", "probability") |>
    tidyr::pivot_wider(names_from = "class", values_from = "probability") |>
    dplyr::select(-"resp_id") |>
    as.matrix() |>
    unname()

  class_diff <- abs(
    round(measr_class, digits = 4) -
      round(ecpe_lldcm$posterior[, c(1, 5, 3, 2, 7, 6, 4, 8)], digits = 4)
  )

  expect_lt(mean(class_diff), .02)
  expect_lt(median(class_diff), .02)

  measr_attr <- ecpe_preds$attribute_probabilities |>
    dplyr::select("resp_id", "attribute", "probability") |>
    tidyr::pivot_wider(names_from = "attribute", values_from = "probability") |>
    dplyr::select(-"resp_id") |>
    as.matrix() |>
    unname()

  attr_diff <- abs(
    round(measr_attr, digits = 4) -
      round(ecpe_lldcm$eap, digits = 4)
  )

  expect_lt(mean(attr_diff), .02)
  expect_lt(median(attr_diff), .02)
})

test_that("ecpe reliability", {
  skip_on_cran()

  ecpe_reli <- reliability(cmds_ecpe_lcdm)
  ecpe_reli8 <- reliability(cmds_ecpe_lcdm, threshold = 0.8)

  # list naming -----
  expect_equal(
    names(ecpe_reli),
    c("pattern_reliability", "map_reliability", "eap_reliability")
  )
  expect_equal(names(ecpe_reli$pattern_reliability), c("p_a", "p_c"))
  expect_equal(names(ecpe_reli$map_reliability), c("accuracy", "consistency"))
  expect_equal(
    names(ecpe_reli$map_reliability$accuracy),
    c(
      "attribute",
      "acc",
      "lambda_a",
      "kappa_a",
      "youden_a",
      "tetra_a",
      "tp_a",
      "tn_a"
    )
  )
  expect_equal(
    names(ecpe_reli$map_reliability$consistency),
    c(
      "attribute",
      "consist",
      "lambda_c",
      "kappa_c",
      "youden_c",
      "tetra_c",
      "tp_c",
      "tn_c",
      "gammak",
      "pc_prime"
    )
  )
  expect_equal(
    names(ecpe_reli$eap_reliability),
    c("attribute", "rho_pf", "rho_bs", "rho_i", "rho_tb")
  )

  # list rows -----
  expect_equal(
    ecpe_reli$map_reliability$accuracy$attribute,
    colnames(dcmdata::ecpe_qmatrix)[-1]
  )
  expect_equal(
    ecpe_reli$map_reliability$consistency$attribute,
    colnames(dcmdata::ecpe_qmatrix)[-1]
  )
  expect_equal(
    ecpe_reli$eap_reliability$attribute,
    colnames(dcmdata::ecpe_qmatrix)[-1]
  )

  # reliability values -----
  patt_diff <- abs(
    round(unname(ecpe_reli$pattern_reliability), digits = 4) -
      round(unname(ecpe_lldcm_reli[[1]]), digits = 4)
  )
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

  # check extraction -----
  expect_equal(cmds_ecpe_lcdm@reliability, list())
  err <- rlang::catch_cnd(measr_extract(
    cmds_ecpe_lcdm,
    "classification_reliability"
  ))
  expect_match(
    err$message,
    "Reliability information must be\\nadded to a model"
  )

  reli_mod <- add_reliability(cmds_ecpe_lcdm)
  expect_equal(reli_mod@reliability, ecpe_reli)

  expect_equal(
    measr_extract(reli_mod, "classification_reliability"),
    dplyr::full_join(
      dplyr::select(
        reli_mod@reliability$map_reliability$accuracy,
        "attribute",
        accuracy = "acc"
      ),
      dplyr::select(
        reli_mod@reliability$map_reliability$consistency,
        "attribute",
        consistency = "consist"
      ),
      by = "attribute"
    )
  )

  # reliability thresholds -----
  expect_equal(ecpe_reli$pattern_reliability, ecpe_reli8$pattern_reliability)
  expect_equal(ecpe_reli$eap_reliability, ecpe_reli$eap_reliability)
  expect_false(identical(
    ecpe_reli$map_reliability$accuracy,
    ecpe_reli8$map_reliability$accuracy
  ))
  expect_false(identical(
    ecpe_reli$map_reliability$consistency,
    ecpe_reli8$map_reliability$consistency
  ))
})

test_that("m2 calculation is correct", {
  skip_on_cran()

  m2 <- fit_m2(cmds_ecpe_lcdm)

  expect_equal(m2$m2, 507.0756, tolerance = 0.1)
  expect_equal(m2$df, 325)
  expect_equal(m2$pval, 0, tolerance = 0.1)
  expect_equal(m2$rmsea, 0.0138, tolerance = 0.1)
  expect_equal(m2$ci_lower, 0.0115, tolerance = 0.1)
  expect_equal(m2$ci_upper, 0.0161, tolerance = 0.1)
  expect_equal(m2$srmsr, 0.0316, tolerance = 0.1)

  m2_mod <- add_fit(cmds_ecpe_lcdm, method = "m2")
  expect_equal(m2_mod@fit$m2, m2)
})

test_that("mcmc requirements error", {
  skip_on_cran()

  err <- rlang::catch_cnd(add_fit(cmds_ecpe_lcdm, method = "ppmc"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`method = \"mcmc\"`")

  err <- rlang::catch_cnd(add_criterion(cmds_ecpe_lcdm))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`method = \"mcmc\"`")
})

test_that("read/write with cmdstanr", {
  dir <- withr::local_tempdir()
  file <- fs::file_temp(tmp_dir = dir)

  # writing -----
  suppressMessages(
    write_measrfit(cmds_ecpe_lcdm, file = file)
  )
  expect_true(fs::file_exists(fs::path_ext_set(file, "rds")))
  expect_true(fs::file_exists(fs::path_ext_set(paste0(file, "-1"), "csv")))
  expect_identical(
    cmds_ecpe_lcdm@model$output_files(),
    as.character(fs::path_ext_set(paste0(file, "-1"), "csv"))
  )

  # read with new specs -----
  clean_data <- rdcmchecks::clean_data(
    dcmdata::ecpe_data,
    identifier = "resp_id",
    missing = NA,
    cleaned_qmatrix = list(
      clean_qmatrix = ecpe_spec@qmatrix,
      attribute_names = ecpe_spec@qmatrix_meta$attribute_names,
      item_identifier = ecpe_spec@qmatrix_meta$item_identifier,
      item_names = ecpe_spec@qmatrix_meta$item_names
    ),
    arg_qmatrix = "ecpe_spec"
  )

  check_fit <- check_previous_fit(
    file = fs::path_ext_set(file, "rds"),
    dcm_spec = ecpe_spec,
    clean_data = clean_data,
    stan_mthd = optim(),
    stan_bknd = rstan()
  )
  expect_null(check_fit)
})
