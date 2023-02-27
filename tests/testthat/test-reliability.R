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
  patt_diff <- abs(round(unname(mdm_reli$pattern_reliability), digits = 4) -
                     round(unname(mdm_lldcm_reli[[1]]), digits = 4))
  expect_lt(mean(patt_diff), .01)
  expect_lt(median(patt_diff), .01)

  map_acc_diff <- abs(
    round(as.matrix(mdm_reli$map_reliability$accuracy[, -1]), digits = 4) -
      round(as.matrix(mdm_lldcm_reli[[2]][1:7]), digits = 4)
  )
  expect_lt(mean(map_acc_diff), .01)
  expect_lt(median(map_acc_diff), .01)

  map_con_diff <- abs(
    round(as.matrix(mdm_reli$map_reliability$consistency[, -1]), digits = 4) -
      round(as.matrix(mdm_lldcm_reli[[2]][8:16]), digits = 4)
  )
  expect_lt(mean(map_con_diff), .01)
  expect_lt(median(map_con_diff), .01)

  eap_diff <- abs(
    round(as.matrix(mdm_reli$eap_reliability[, -1]), digits = 4) -
      round(as.matrix(mdm_lldcm_reli[[3]]), digits = 4)
  )
  expect_lt(mean(eap_diff), .01)
  expect_lt(median(eap_diff), .01)
})
