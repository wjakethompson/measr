restructure_upars <- bridgesampling:::.restructure_upars
bridge_sampler_normal <- bridgesampling:::.bridge.sampler.normal
cmdstan_log_posterior <- bridgesampling:::.cmdstan_log_posterior

#' @importFrom bridgesampling bridge_sampler
#' @export
bridgesampling::bridge_sampler

bridge_sampler.measrfit <- function (samples = NULL, repetitions = 1,
                                     method = "normal", cores = 1,
                                     use_neff = FALSE, maxiter = 1000,
                                     silent = FALSE, verbose = FALSE, ...)
{
  if (!requireNamespace("cmdstanr"))
    stop("package cmdstanr required")
  if (is.null(samples$.__enclos_env__$private$model_methods_env_$model_ptr)) {
    samples$init_model_methods()
  }
  samples_md <- samples$metadata()
  upars <- samples$unconstrain_draws()
  samples_dims <- c(sum(unlist(samples_md$stan_variable_sizes[!samples_md$stan_variables %in%
                                                                c("lp__", "log_lik")])), samples_md$iter_sampling, length(samples_md$id))
  upars <- array(unlist(upars), dim = samples_dims)
  upars_args <- restructure_upars(upars, use_neff)
  bs_args <- list(log_posterior = cmdstan_log_posterior,
                  data = list(stanfit = samples),
                  repetitions = repetitions, cores = cores, packages = "rstan",
                  maxiter = maxiter, silent = silent, verbose = verbose,
                  r0 = 0.5, tol1 = 1e-10, tol2 = 1e-04)

  if (cores == 1) {
    bridge_output <- do.call(what = bridge_sampler_normal,
                             args = c(upars_args, bs_args))
  }
  else {
    bridge_output <- do.call(what = bridge_sampler_normal,
                             args = c(upars_args, bs_args,
                                      list(varlist = "stanfit",
                                           envir = sys.frame(sys.nframe()))))
  }
  return(bridge_output)
}
