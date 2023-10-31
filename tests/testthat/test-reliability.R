test_that("dino reliability", {
  dino_reli <- reliability(rstn_dino)

  # list naming
  expect_equal(names(dino_reli), c("pattern_reliability", "map_reliability",
                                  "eap_reliability"))
  expect_equal(names(dino_reli$pattern_reliability), c("p_a", "p_c"))
  expect_equal(names(dino_reli$map_reliability), c("accuracy", "consistency"))
  expect_equal(names(dino_reli$map_reliability$accuracy),
               c("attribute", "acc", "lambda_a", "kappa_a", "youden_a",
                 "tetra_a", "tp_a", "tn_a"))
  expect_equal(names(dino_reli$map_reliability$consistency),
               c("attribute", "consist", "lambda_c", "kappa_c", "youden_c",
                 "tetra_c", "tp_c", "tn_c", "gammak", "pc_prime"))
  expect_equal(names(dino_reli$eap_reliability),
               c("attribute", "rho_pf", "rho_bs", "rho_i", "rho_tb"))

  # list rows
  expect_equal(dino_reli$map_reliability$accuracy$attribute,
               colnames(q_matrix)[-1])
  expect_equal(dino_reli$map_reliability$consistency$attribute,
               colnames(q_matrix)[-1])
  expect_equal(dino_reli$eap_reliability$attribute,
               colnames(q_matrix)[-1])
})

test_that("reliability can be added to model object", {
  dina_mod <- rstn_dina
  expect_equal(dina_mod$reliability, list())

  err <- rlang::catch_cnd(measr_extract(dina_mod, "pattern_reliability"))
  expect_match(err$message, "Reliability information must be added to a model")
  err <- rlang::catch_cnd(measr_extract(dina_mod, "classification_reliability"))
  expect_match(err$message, "Reliability information must be added to a model")
  err <- rlang::catch_cnd(measr_extract(dina_mod, "probability_reliability"))
  expect_match(err$message, "Reliability information must be added to a model")

  dina_mod <- add_reliability(dina_mod)
  expect_equal(names(dina_mod$reliability),
               c("pattern_reliability", "map_reliability", "eap_reliability"))

  expect_equal(
    measr_extract(dina_mod, "pattern_reliability"),
    tibble(accuracy = dina_mod$reliability$pattern_reliability[["p_a"]],
           consistency = dina_mod$reliability$pattern_reliability[["p_c"]])
  )

  expect_equal(measr_extract(dina_mod, "classification_reliability"),
               dplyr::full_join(
                 dplyr::select(dina_mod$reliability$map_reliability$accuracy,
                               "attribute", accuracy = "acc"),
                 dplyr::select(dina_mod$reliability$map_reliability$consistency,
                               "attribute", consistency = "consist"),
                 by = "attribute"
               ))

  expect_equal(measr_extract(dina_mod, "classification_reliability",
                             agreement = c("lambda", "tn")),
               dplyr::full_join(
                 dplyr::select(dina_mod$reliability$map_reliability$accuracy,
                               "attribute", accuracy = "acc",
                               "lambda_a", "tn_a"),
                 dplyr::select(dina_mod$reliability$map_reliability$consistency,
                               "attribute", consistency = "consist",
                               "lambda_c", "tn_c"),
                 by = "attribute"
               ))

  expect_equal(measr_extract(dina_mod, "probability_reliability"),
               dplyr::select(dina_mod$reliability$eap_reliability,
                             "attribute", informational = "rho_i"))

  expect_equal(measr_extract(dina_mod, "probability_reliability",
                             agreement = "bs"),
               dplyr::select(dina_mod$reliability$eap_reliability,
                             "attribute", informational = "rho_i", "rho_bs"))

  expect_identical(dina_mod$reliability, reliability(dina_mod))
})
