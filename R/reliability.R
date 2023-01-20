reliability <- function(x) {
  UseMethod("reliability")
}

reliability.measrdcm <- function(x) {
  # coerce model into a list of values required for reliability
  obj <- if (x$method == "mcmc") {
    create_reli_list_mcmc(x)
  } else if (x$method == "optim") {
    create_reli_list_optim(x)
  }


}
