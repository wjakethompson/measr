# Log marginal likelihood calculation

Calculate the log marginal likelihood with bridge sampling (Meng & Wong,
1996). This is a wrapper around
[`bridgesampling::bridge_sampler()`](https://rdrr.io/pkg/bridgesampling/man/bridge_sampler.html).
Therefore, log marginal likelihood calculation is currently only
available for models estimated with `{rstan}` using MCMC.

## Usage

``` r
log_mll(x, ..., force = FALSE)
```

## Arguments

- x:

  A [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) object
  estimated with `backend = "optim"`.

- ...:

  Unused.

- force:

  If the criterion has already been added to the model object with
  [`add_criterion()`](https://measr.info/dev/reference/model_evaluation.md),
  should it be recalculated. Default is `FALSE`.

## Value

The estimate of the log marginal likelihood.

## References

Meng, X.-L., & Wong, W. H. (1996). Simulating ratios of normalizing
constants via a simple identity: A theoretical exploration. *Statistical
Sinica, 6*(4), 831-860. <https://www.jstor.org/stable/24306045>

## Examples

``` r
model_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
                          identifier = "item")
model <- dcm_estimate(dcm_spec = model_spec, data = dcmdata::mdm_data,
                      identifier = "respondent", method = "mcmc",
                      seed = 63277)
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0.00013 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.3 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 4000 [  0%]  (Warmup)
#> Chain 1: Iteration:  400 / 4000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  800 / 4000 [ 20%]  (Warmup)
#> Chain 1: Iteration: 1200 / 4000 [ 30%]  (Warmup)
#> Chain 1: Iteration: 1600 / 4000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 2000 / 4000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 2001 / 4000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 2400 / 4000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 2800 / 4000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 3200 / 4000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 3600 / 4000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 4000 / 4000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 7.398 seconds (Warm-up)
#> Chain 1:                7.26 seconds (Sampling)
#> Chain 1:                14.658 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.000125 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.25 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 4000 [  0%]  (Warmup)
#> Chain 2: Iteration:  400 / 4000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  800 / 4000 [ 20%]  (Warmup)
#> Chain 2: Iteration: 1200 / 4000 [ 30%]  (Warmup)
#> Chain 2: Iteration: 1600 / 4000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 2000 / 4000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 2001 / 4000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 2400 / 4000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 2800 / 4000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 3200 / 4000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 3600 / 4000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 4000 / 4000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 7.556 seconds (Warm-up)
#> Chain 2:                9.011 seconds (Sampling)
#> Chain 2:                16.567 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 0.000125 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.25 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 4000 [  0%]  (Warmup)
#> Chain 3: Iteration:  400 / 4000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  800 / 4000 [ 20%]  (Warmup)
#> Chain 3: Iteration: 1200 / 4000 [ 30%]  (Warmup)
#> Chain 3: Iteration: 1600 / 4000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 2000 / 4000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 2001 / 4000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 2400 / 4000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 2800 / 4000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 3200 / 4000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 3600 / 4000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 4000 / 4000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 7.42 seconds (Warm-up)
#> Chain 3:                6.114 seconds (Sampling)
#> Chain 3:                13.534 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 0.00013 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.3 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 4000 [  0%]  (Warmup)
#> Chain 4: Iteration:  400 / 4000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  800 / 4000 [ 20%]  (Warmup)
#> Chain 4: Iteration: 1200 / 4000 [ 30%]  (Warmup)
#> Chain 4: Iteration: 1600 / 4000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 2000 / 4000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 2001 / 4000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 2400 / 4000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 2800 / 4000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 3200 / 4000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 3600 / 4000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 4000 / 4000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 7.704 seconds (Warm-up)
#> Chain 4:                7.127 seconds (Sampling)
#> Chain 4:                14.831 seconds (Total)
#> Chain 4: 
#> Warning: There were 2 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems

log_mll(model)
#> [1] -346.3355
```
