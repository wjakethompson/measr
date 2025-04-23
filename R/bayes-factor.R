bayes_factor <- function(mod1, mod2) {
  if (mod1$method == "optim" || mod2$method == "optim") {
    rlang::abort("error_bad_method",
                 message = glue::glue("Bayes factor is only ",
                                      "available for models estimated with ",
                                      "`method = \"mcmc\"`."))
  }

  mod1 <- check_model(mod1, required_class = "measrfit", name = "model")
  mod2 <- check_model(mod2, required_class = "measrfit", name = "model")

  mod1 <- add_marginal_likelihood(mod1)
  mod2 <- add_marginal_likelihood(mod2)

  log_marg_lik1 <- extract_marginal_likelihood(mod1)
  log_marg_lik2 <- extract_marginal_likelihood(mod2)

  marg_lik1 <- 1 / (1 + exp(-log_marg_lik1))
  marg_lik2 <- 1 / (1 + exp(-log_marg_lik2))

  bf <- marg_lik1 / marg_lik2

  return(bf)
}

mod1 <- measr_dcm(data = measr:::dino_data, missing = NA, qmatrix = measr:::q_matrix,
                        resp_id = "resp_id", item_id = "item", type = "dino",
                        method = "mcmc", seed = 63277, backend = "cmdstanr",
                        precompiled = measr:::stanmodels$test_dina,
                  iter_warmup = 100, iter_sampling = 300, chains = 2,
                  parallel_chains = 2, refresh = 1, sig_figs = 12)

mod2 <- measr_dcm(data = measr:::dino_data, missing = NA, qmatrix = measr:::q_matrix,
                        resp_id = "resp_id", item_id = "item", type = "dina",
                        method = "mcmc", seed = 63277, backend = "cmdstanr",
                        precompiled = measr:::stanmodels$test_dina,
                  iter_warmup = 10, iter_sampling = 20, chains = 2,
                  parallel_chains = 2, refresh = 1, sig_figs = 12)

x1 <- mod1
y1 <- mod2
