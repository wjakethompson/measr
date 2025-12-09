# Bayes factor for model comparisons

Calculate the Bayes factor for model comparisons, which represents the
posterior odds of the null hypothesis when the prior probability of the
null model is 0.5 (Jeffreys, 1935; Kass & Raftery, 1995). Consistent
with the Bayesian reporting guidelines from Kruschke (2021), we
calculate the posterior probability of the null model for a variety of
prior probabilities, in addition to the Bayes factor.

## Usage

``` r
bayes_factor(
  x,
  ...,
  model_names = NULL,
  prior_prob = seq(0.02, 0.98, by = 0.02)
)
```

## Arguments

- x:

  A [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) object.

- ...:

  Additional
  [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) to be
  compared to `x`.

- model_names:

  Names given to each provided model in the comparison output. If `NULL`
  (the default), the names will be parsed from the names of the objects
  passed for comparison.

- prior_prob:

  A numeric vector of prior probabilities for the null model used to
  calculate the posterior probability of the null model relative to
  alternative model. See details for more information.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per model comparison and four columns.

- `null_model`: The null model in the comparison.

- `alt_model`: The alternative model in the comparison.

- `bf`: The estimated Bayes factor.

- `posterior_probs`: A nested list column, where element element is a
  tibble with two columns:

  - `prior_prob_null`: The prior probability that the null model is
    correct.

  - `posterior_prob_null`: The posterior probability that the null model
    is correct.

  The list column can be unnested with
  [`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)
  (see examples). If `prior_prob` is `NULL`, the `posterior_probs`
  column is excluded from the returned object.

## Details

Bayes factors will be calculated for all possible pairwise comparisons
between the models provided to `x` and `...`. In each comparison, one
model is identified as the null model, and the other is the alternative.
This distinction is not terribly meaningful from a calculation
standpoint, as the probabilities for the alternative model are simply 1
minus the null probabilities. If you want particular models to be
labeled as the "null", the determination is made by the order the models
are sent to the function. That is, `x` will always be the null model.
The first model included in `...` will be the alternative model when
compared to `x` and the null model when compared to all other models
included in `...`. Similarly, the second model included in `...` will be
the alternative model when compared to `x` and the first model included
in `...` and the null model in all other comparisons.

`prior_prob` is used to specify a vector of possible prior probabilities
for the null model. These are used in conjunction with the Bayes factor
to determine the posterior model probability for the null model,
relative to the alternative model. The posterior probability for the
alternative model can be calculated as 1 minus the null model's
posterior probability. You may specify a specific prior probability, or
specify a range of possibilities to construct a graph similar to
Kruschke's (2021) Figure 1. These probabilities can be interpreted as,
"If the prior probability is {`prior_prob_null`}, then the posterior is
{`posterior_prob_null`}" (or 1 minus for the alternative model).

## References

Jeffreys, H. (1935). Some tests of significance, treated by the theory
of probability. *Mathematical Proceedings of the Cambridge Philosophical
Society, 31*(2), 203-222.
[doi:10.1017/S030500410001330X](https://doi.org/10.1017/S030500410001330X)

Kass, R. E., & Raftery, A. E. (1995). Bayes factors. *Journal of the
American Statistical Association, 90*(430), 773-795.
[doi:10.1080/01621459.1995.10476572](https://doi.org/10.1080/01621459.1995.10476572)

Kruschke, J. K. (2021). Bayesian analysis reporting guidelines. *Nature,
5*, 1282-1291.
[doi:10.1038/s41562-021-01177-7](https://doi.org/10.1038/s41562-021-01177-7)

## Examples

``` r
mdm_dina <- dcm_estimate(
  dcm_specify(dcmdata::mdm_qmatrix, identifier = "item",
              measurement_model = dina()),
  data = dcmdata::mdm_data, missing = NA, identifier = "respondent",
  method = "mcmc", seed = 63277, backend = "rstan",
  iter = 700, warmup = 500, chains = 2, refresh = 0
)
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

mdm_dino <- dcm_estimate(
  dcm_specify(dcmdata::mdm_qmatrix, identifier = "item",
              measurement_model = dino()),
  data = dcmdata::mdm_data, missing = NA, identifier = "respondent",
  method = "mcmc", seed = 63277, backend = "rstan",
  iter = 700, warmup = 500, chains = 2, refresh = 0
)
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

bf <- bayes_factor(mdm_dina, mdm_dino)
bf
#> # A tibble: 1 × 4
#>   null_model alt_model    bf posterior_probs  
#>   <chr>      <chr>     <dbl> <list>           
#> 1 mdm_dina   mdm_dino  0.972 <tibble [49 × 2]>

tidyr::unnest(bf, "posterior_probs")
#> # A tibble: 49 × 5
#>    null_model alt_model    bf prior_prob_null posterior_prob_null
#>    <chr>      <chr>     <dbl>           <dbl>               <dbl>
#>  1 mdm_dina   mdm_dino  0.972            0.02              0.0212
#>  2 mdm_dina   mdm_dino  0.972            0.04              0.0424
#>  3 mdm_dina   mdm_dino  0.972            0.06              0.0635
#>  4 mdm_dina   mdm_dino  0.972            0.08              0.0845
#>  5 mdm_dina   mdm_dino  0.972            0.1               0.106 
#>  6 mdm_dina   mdm_dino  0.972            0.12              0.127 
#>  7 mdm_dina   mdm_dino  0.972            0.14              0.147 
#>  8 mdm_dina   mdm_dino  0.972            0.16              0.168 
#>  9 mdm_dina   mdm_dino  0.972            0.18              0.189 
#> 10 mdm_dina   mdm_dino  0.972            0.2               0.210 
#> # ℹ 39 more rows
```
