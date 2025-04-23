# algorithm --------------------------------------------------------------------
get_algorithm <- S7::new_generic("get_algorithm", c("backend", "method"))

S7::method(get_algorithm, list(stanbackend, optim)) <-
  function(backend, method, ..., args) {
    args$algorithm
  }

S7::method(get_algorithm, list(rstan, mcmc)) <-
  function(backend, method, ..., model) {
    model@stan_args[[1]][["algorithm"]]
  }

S7::method(get_algorithm, list(cmdstanr, mcmc)) <-
  function(backend, method, ..., model) {
    model$metadata()$algorithm
  }


# version info -----------------------------------------------------------------
get_version_info <- S7::new_generic("get_version_info", "backend")

S7::method(get_version_info, rstan) <- function(backend) {
  list(R = base::getRversion(),
       `R-measr` = utils::packageVersion("measr"),
       `R-rstan` = utils::packageVersion("rstan"),
       `R-StanHeaders` = utils::packageVersion("StanHeaders"),
       Stan = as.package_version(rstan::stan_version()))
}

S7::method(get_version_info, cmdstanr) <- function(backend) {
  list(R = base::getRversion(),
       `R-measr` = utils::packageVersion("measr"),
       `R-cmdstanr` = utils::packageVersion("cmdstanr"),
       CmdStan = as.package_version(cmdstanr::cmdstan_version()))
}
