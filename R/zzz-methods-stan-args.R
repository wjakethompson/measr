default_stan_args <- S7::new_generic("default_stan_args",
                                     c("backend", "method"))

# methods ----------------------------------------------------------------------
S7::method(default_stan_args, list(rstan, mcmc)) <-
  function(backend, method, ..., user_args = list()) {
    user_names <- names(user_args)
    new_control <- if ("control" %in% user_names) {
      utils::modifyList(list(adapt_delta = 0.95), user_args$control)
    } else {
      list(adapt_delta = 0.95)
    }

    default_iter <- ifelse("iter" %in% user_names, user_args$iter, 4000)
    default_warmup <- ifelse("warmup" %in% user_names, user_args$warmup,
                             default_iter / 2)

    list(iter = default_iter, warmup = default_warmup, chains = 4,
         cores = getOption("mc.cores", 1L), control = new_control)
  }

S7::method(default_stan_args, list(cmdstanr, mcmc)) <-
  function(backend, method, ...) {
    list(iter_sampling = 2000, iter_warmup = 2000, chains = 4,
         parallel_chains = getOption("mc.cores", 1L),
         adapt_delta = 0.95)
  }

S7::method(default_stan_args, list(rstan, optim)) <-
  function(backend, method, ...) {
    list(algorithm = "LBFGS")
  }

S7::method(default_stan_args, list(cmdstanr, optim)) <-
  function(backend, method, ...) {
    list(algorithm = "lbfgs")
  }

S7::method(default_stan_args, list(rstan, gqs)) <-
  function(backend, method, ..., draws = NULL) {
    list(draws = posterior::as_draws_matrix(draws))
  }

S7::method(default_stan_args, list(cmdstanr, gqs)) <-
  function(backend, method, ..., draws = NULL) {
    list(fitted_params = draws)
  }
