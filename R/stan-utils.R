create_stan_data <- function(dat, qmat, type) {
  ragged_array <- dat %>%
    tibble::rowid_to_column() %>%
    dplyr::group_by(.data$resp_id) %>%
    dplyr::summarize(start = min(.data$rowid),
                     num = dplyr::n()) %>%
    dplyr::arrange(.data$resp_id)

  profiles <- create_profiles(ncol(qmat))
  xi <- calc_xi(alpha = profiles, qmatrix = qmat, type = type)

  stan_data <- list(
    I = nrow(qmat),
    R = length(unique(dat$resp_id)),
    N = nrow(dat),
    C = 2 ^ ncol(qmat),
    A = ncol(qmat),
    ii = as.numeric(dat$item_id),
    rr = as.numeric(dat$resp_id),
    y = dat$score,
    start = ragged_array$start,
    num = ragged_array$num,
    Alpha = profiles,
    Xi = xi
  )

  return(stan_data)
}

create_stan_params <- function(backend, method, ...) {
  ## user defined
  user_pars <- list(...)
  user_names <- names(user_pars)
  if ("control" %in% user_names) {
    new_control <- utils::modifyList(list(adapt_delta = 0.95),
                                     user_pars$control)
    user_pars$control <- new_control
  } else if (backend == "rstan" && method == "mcmc") {
    user_pars$control <- list(adapt_delta = 0.95)
  }

  ## some reasonable defaults
  if (method == "mcmc") {
    if (backend == "rstan") {
      defl_iter <- ifelse("iter" %in% user_names, user_pars$iter, 4000)
      defl_warmup <- ifelse("warmup" %in% user_names, user_pars$warmup,
                            defl_iter / 2)
      defl_pars <- list(iter = defl_iter, warmup = defl_warmup, chains = 4,
                        cores = getOption("mc.cores", 1L))
    } else if (backend == "cmdstanr") {
      defl_pars <- list(iter_sampling = 2000, iter_warmup = 2000, chains = 4,
                        parallel_chains = getOption("mc.cores", 1L),
                        adapt_delta = 0.95)
    }
  } else if (method == "optim") {
    defl_pars <- list(algorithm = ifelse(backend == "rstan", "LBFGS", "lbfgs"))
  }
  stan_pars <- utils::modifyList(defl_pars, user_pars)

  return(stan_pars)
}

create_stan_gqs_params <- function(backend, draws) {
  stan_pars <- if (backend == "rstan") {
    list(draws = posterior::as_draws_matrix(draws))
  } else if (backend == "cmdstanr") {
    list(fitted_params = draws)
  }

  return(stan_pars)
}

create_stan_function <- function(backend, method, code, precompiled = NULL,
                                 pars, silent = 1) {
  if (backend == "rstan") {
    if (is.null(precompiled)) {
      out <- utils::capture.output( #nolint
        comp_mod <- eval_silent(
          rstan::stan_model(model_code = code$stancode),
          type = "message", try = TRUE, silent = silent >= 2
        )
      )
      pars$object <- comp_mod
    } else {
      pars$object <- precompiled
    }

    fit_func <- switch(method,
                       mcmc = rstan::sampling,
                       optim = rstan::optimizing,
                       gqs = rstan::gqs)
  } else if (backend == "cmdstanr") {
    comp_mod <- cmdstanr::cmdstan_model(
      cmdstanr::write_stan_file(code$stancode),
      compile = FALSE
    )
    if (cmdstanr::cmdstan_version() >= "2.29.0") {
      comp_mod$format(
        canonicalize = list("deprecations", "braces", "parentheses"),
        overwrite_file = TRUE, quiet = TRUE, backup = FALSE
      )
    }
    comp_mod <- eval_silent(
      comp_mod$compile(quiet = TRUE),
      type = "message", try = TRUE, silent = silent >= 2
    )

    fit_func <- switch(method,
                       mcmc = comp_mod$sample,
                       optim = comp_mod$optimize,
                       gqs = comp_mod$generate_quantities)
  }
  return(list(func = fit_func, pars = pars))
}

model_matrix_name_repair <- function(x) {
  x <- gsub("\\(|\\)", "", x)
  x <- gsub(":", "__", x)
  x <- tolower(x)

  return(x)
}

one_down_params <- function(x, item) {
  all_atts <- strsplit(x, split = "__")[[1]]
  if (length(all_atts) <= 1) return("")
  att_combos <- utils::combn(all_atts, m = length(all_atts) - 1,
                             simplify = FALSE)

  paste("l", item, "_", length(all_atts) - 1,
        sapply(att_combos, paste, collapse = ""), sep = "", collapse = ",")
}
one_down_params <- Vectorize(one_down_params, USE.NAMES = FALSE)


define_interactions <- function(param_level, param_info) {
  vector_def <- param_info %>% #nolint
    glue::glue_data("vector[{num_comp}] {gsub('l', 'v', param_name)} = ",
                    "[{comp_atts}]';")
  interaction_constrain <- param_info %>% #nolint
    glue::glue_data("real {param_name} = exp({param_name}_raw) - ",
                    "min({gsub('l', 'v', param_name)});")

  trans_par_code <- glue::glue(
    "  ////////////////////////////////// {param_level}-way interactions",
    "  {glue::glue_collapse(vector_def, sep = \"\n  \")}",
    "",
    "  ////////////////////////////////// constrain {param_level}-way",
    "  {glue::glue_collapse(interaction_constrain, sep = \"\n  \")}",
    .sep = "\n", .trim = FALSE
  )

  return(trans_par_code)
}
