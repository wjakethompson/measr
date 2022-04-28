test_that("LCDM with ECPE works", {
  lcdm_script <-
    lcdm_script(qmatrix = dplyr::select(ecpe_qmatrix, -.data$item_id))

  stan_mod <- rstan::stan_model(model_code = lcdm_script$stancode)
  expect_s4_class(stan_mod, "stanmodel")

  dat <- ecpe_data %>%
    tidyr::pivot_longer(-.data$resp_id, values_to = "score",
                        names_to = "item", names_prefix = "E") %>%
    dplyr::mutate(item = as.integer(.data$item)) %>%
    dplyr::arrange(.data$resp_id, .data$item)

  stan_dat <- list(I = length(unique(dat$item)),
                   R = length(unique(dat$resp_id)),
                   N = nrow(dat),
                   C = 8L,
                   A = 3L,
                   ii = dat$item,
                   rr = dat$resp_id,
                   y = dat$score,
                   start = seq(1, nrow(dat), by = 28),
                   num = rep(28, length(unique(dat$resp_id))),
                   Alpha = matrix(c(0, 0, 0,
                                    1, 0, 0,
                                    0, 1, 0,
                                    0, 0, 1,
                                    1, 1, 0,
                                    1, 0, 1,
                                    0, 1, 1,
                                    1, 1, 1), ncol = 3, byrow = TRUE),
                   Xi = matrix(rep(1, 224), nrow = length(unique(dat$item)),
                               ncol = 8L))

  mod <- rstan::optimizing(stan_mod, data = stan_dat, seed = 1008)
})
