# Relative fit for Bayesian models

For models estimated with MCMC, relative model fit comparisons can be
made using the LOO-CV or WAIC indicates (Vehtari et al., 2017). These
functions are wrappers for the
[loo](https://mc-stan.org/loo/reference/loo-package.html) package. See
the loo package [vignettes](https://mc-stan.org/loo/articles/) for
details on the implementation.

## Usage

``` r
# S3 method for class '`measr::measrdcm`'
loo(x, ..., r_eff = NA, force = FALSE)

# S3 method for class '`measr::measrdcm`'
waic(x, ..., force = FALSE)

# S3 method for class '`measr::measrdcm`'
loo_compare(x, ..., criterion = c("loo", "waic"), model_names = NULL)
```

## Arguments

- x:

  A [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) object.

- ...:

  For [`loo()`](https://mc-stan.org/loo/reference/loo.html) and
  [`waic()`](https://mc-stan.org/loo/reference/waic.html), additional
  arguments passed to
  [`loo::loo.array()`](https://mc-stan.org/loo/reference/loo.html) or
  [`loo::waic.array()`](https://mc-stan.org/loo/reference/waic.html),
  respectively. For
  [`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html),
  additional
  [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) objects
  to be compared to `x`.

- r_eff:

  Vector of relative effective sample size estimates for the likelihood
  (`exp(log_lik)`) of each observation. This is related to the relative
  efficiency of estimating the normalizing term in self-normalized
  importance sampling when using posterior draws obtained with MCMC. If
  MCMC draws are used and `r_eff` is not provided then the reported PSIS
  effective sample sizes and Monte Carlo error estimates can be
  over-optimistic. If the posterior draws are (near) independent then
  `r_eff=1` can be used. `r_eff` has to be a scalar (same value is used
  for all observations) or a vector with length equal to the number of
  observations. The default value is 1. See the
  [`relative_eff()`](https://mc-stan.org/loo/reference/relative_eff.html)
  helper functions for help computing `r_eff`.

- force:

  If the LOO criterion has already been added to the model object with
  [`add_criterion()`](https://measr.info/dev/reference/model_evaluation.md),
  should it be recalculated. Default is `FALSE`.

- criterion:

  The name of the criterion to be extracted from the `x` for comparison.

- model_names:

  Names given to each provided model in the comparison output. If `NULL`
  (the default), the names will be parsed from the names of the objects
  passed for comparison.

## Value

For [`loo()`](https://mc-stan.org/loo/reference/loo.html) and
[`waic()`](https://mc-stan.org/loo/reference/waic.html), the information
criteria returned by
[`loo::loo.array()`](https://mc-stan.org/loo/reference/loo.html) or
[`loo::waic.array()`](https://mc-stan.org/loo/reference/waic.html),
respectively.

For
[`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html),
the criterion comparison returned by
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html).

## References

Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing, 27*(5), 1413-1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)
