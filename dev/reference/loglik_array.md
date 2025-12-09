# Extract the log-likelihood of an estimated model

The `loglik_array()` methods for
[measrdcm](https://measr.info/dev/reference/dcm_estimate.md) objects
calculates the log-likelihood for an estimated model via the generated
quantities functionality in *Stan* and returns the draws of the
`log_lik` parameter.

## Usage

``` r
loglik_array(model, ...)
```

## Arguments

- model:

  A [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) object.

- ...:

  Unused. For future extensions.

## Value

A
"[`draws_array`](https://mc-stan.org/posterior/reference/draws_array.html)"
object containing the log-likelihood estimates for the model.
