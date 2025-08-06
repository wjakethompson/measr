# compile stan model -----------------------------------------------------------
stan_model <- S7::new_generic(
  "stan_model",
  "backend",
  function(backend, code, ...) {
    S7::S7_dispatch()
  }
)

S7::method(stan_model, rstan) <-
  function(backend, code, ..., precompiled = NULL) {
    if (!is.null(precompiled)) {
      return(precompiled)
    }

    # fmt: skip
    out <- utils::capture.output( # nolint
      compiled_model <- eval_silent(rstan::stan_model(model_code = code))
    )
    compiled_model
  }

S7::method(stan_model, cmdstanr) <- function(backend, code, ...) {
  compiled_model <- cmdstanr::cmdstan_model(
    cmdstanr::write_stan_file(code),
    compile = FALSE
  )

  if (cmdstanr::cmdstan_version() >= "2.29.0") {
    compiled_model$format(
      canonicalize = list("deprecations", "braces", "parentheses"),
      overwrite_file = TRUE,
      quiet = TRUE,
      backup = FALSE
    )
  }

  compiled_model <- eval_silent(compiled_model$compile(quiet = TRUE))
  compiled_model
}


# identify stan estimation function --------------------------------------------
stan_function <- S7::new_generic("stan_function", c("backend", "method"))

S7::method(stan_function, list(rstan, mcmc)) <-
  function(backend, method, ...) {
    rstan::sampling
  }
S7::method(stan_function, list(rstan, optim)) <-
  function(backend, method, ...) {
    rstan::optimizing
  }
S7::method(stan_function, list(rstan, gqs)) <-
  function(backend, method, ...) {
    rstan::gqs
  }

S7::method(stan_function, list(cmdstanr, mcmc)) <-
  function(backend, method, ..., compiled_model = NULL) {
    compiled_model$sample
  }
S7::method(stan_function, list(cmdstanr, optim)) <-
  function(backend, method, ..., compiled_model = NULL) {
    compiled_model$optimize
  }
S7::method(stan_function, list(cmdstanr, gqs)) <-
  function(backend, method, ..., compiled_model = NULL) {
    compiled_model$generate_quantities
  }


# final stan calls -------------------------------------------------------------
stan_call <- S7::new_generic(
  "stan_call",
  c("backend", "method"),
  function(backend, method, code, args, ...) {
    S7::S7_dispatch()
  }
)

S7::method(stan_call, list(rstan, stanmethod)) <-
  function(backend, method, code, args, ..., precompiled = NULL) {
    args$object <- stan_model(
      backend = backend,
      code = code,
      precompiled = precompiled
    )
    fit_func <- stan_function(backend = backend, method = method)

    list(call_function = fit_func, args = args)
  }

S7::method(stan_call, list(cmdstanr, stanmethod)) <-
  function(backend, method, code, args, ...) {
    compiled_model <- stan_model(backend = backend, code = code)
    fit_func <- stan_function(
      backend = backend,
      method = method,
      compiled_model = compiled_model
    )

    list(call_function = fit_func, args = args)
  }
