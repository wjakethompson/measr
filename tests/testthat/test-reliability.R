test_that("mdm reliability", {
  mdm_reli <- reliability(rstn_mdm_lcdm)

  # list naming
  expect_equal(names(mdm_reli), c("pattern_reliability", "map_reliability",
                                  "eap_reliability"))
  expect_equal(names(mdm_reli$pattern_reliability), c("p_a", "p_c"))
  expect_equal(names(mdm_reli$map_reliability), c("accuracy", "consistency"))
  expect_equal(names(mdm_reli$map_reliability$accuracy),
               c("attribute", "acc", "lambda_a", "kappa_a", "youden_a",
                 "tetra_a", "tp_a", "tn_a"))
  expect_equal(names(mdm_reli$map_reliability$consistency),
               c("attribute", "consist", "lambda_c", "kappa_c", "youden_c",
                 "tetra_c", "tp_c", "tn_c", "gammak", "pc_prime"))
  expect_equal(names(mdm_reli$eap_reliability),
               c("attribute", "rho_pf", "rho_bs", "rho_i", "rho_tb"))

  # list rows
  expect_equal(mdm_reli$map_reliability$accuracy$attribute,
               colnames(mdm_qmatrix)[-1])
  expect_equal(mdm_reli$map_reliability$consistency$attribute,
               colnames(mdm_qmatrix)[-1])
  expect_equal(mdm_reli$eap_reliability$attribute,
               colnames(mdm_qmatrix)[-1])

  # reliability values
  expect_equal(unname(mdm_reli$pattern_reliability),
               unname(mdm_lldcm_reli[[1]]),
               tolerance = 0.01)
  expect_equal(unname(as.matrix(mdm_reli$map_reliability$accuracy[, -1])),
               unname(as.matrix(mdm_lldcm_reli[[2]][1:7])),
               tolerance = 0.01)
  expect_equal(unname(as.matrix(mdm_reli$map_reliability$consistency[, -1])),
               unname(as.matrix(mdm_lldcm_reli[[2]][8:16])),
               tolerance = 0.01)
  expect_equal(unname(as.matrix(mdm_reli$eap_reliability[, -1])),
               unname(as.matrix(mdm_lldcm_reli[[3]])),
               tolerance = 0.01)
})
