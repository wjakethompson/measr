get_draws <- S7::new_generic("get_draws", "model",
                             function(model, vars = NULL, ...) {
                               S7::S7_dispatch()
                             })

extract_stan_draws <-
  S7::new_generic("extract_stan_draws", c("backend", "method"),
                  function(backend, method, model, vars, ...) {
                    S7::S7_dispatch()
                  })

# methods ----------------------------------------------------------------------
S7::method(get_draws, measrdcm) <- function(model, vars = NULL, ...,
                                            ndraws = NULL) {
  draw_array <- extract_stan_draws(backend = model@backend,
                                   method = model@method,
                                   model = model, vars = vars)

  if (!is.null(ndraws)) {
    keep_draws <- sample(posterior::draw_ids(draw_array), size = ndraws,
                         replace = FALSE)
    draw_array <- posterior::subset_draws(posterior::merge_chains(draw_array),
                                          draw = keep_draws)
  }

  if ("pi" %in% vars) {
    draw_array <- constrain_pi(draw_array, vars = vars)
  }

  draw_array
}

S7::method(extract_stan_draws, list(rstan, mcmc)) <-
  function(backend, method, model, vars) {
    posterior::as_draws_array(model@model) |>
      posterior::subset_draws(variable = vars)
  }

S7::method(extract_stan_draws, list(rstan, optim)) <-
  function(backend, method, model, vars) {
    posterior::as_draws_array(t(as.matrix(model@model$par))) |>
      posterior::subset_draws(variable = vars)
  }

S7::method(extract_stan_draws, list(rstan, gqs)) <-
  function(backend, method, model, vars) {
    posterior::as_draws_array(as.array(model, pars = vars))
  }

S7::method(extract_stan_draws, list(cmdstanr, gqs)) <-
  function(backend, method, model, vars) {
    model$draws(variables = vars, format = "draws_array")
  }

S7::method(extract_stan_draws, list(cmdstanr, stanmethod)) <-
  function(backend, method, model, vars) {
    model@model$draws(variables = vars, format = "draws_array")
  }


# utilities --------------------------------------------------------------------
constrain_01 <- function(x) {
  max(min(x, 0.99999), 0.00001)
}

constrain_pi <- function(draw_array, vars) {
  posterior::bind_draws(
    posterior::subset_draws(draw_array, variable = setdiff(vars, "pi")),
    apply(posterior::subset_draws(draw_array, variable = "pi"),
          c(1, 2, 3), constrain_01) |>
      posterior::as_draws_array()
  )
}
