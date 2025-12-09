# Posterior predictive model checks for assessing model fit

For models estimated with `method = "mcmc"`, use the posterior
distributions to compute expected distributions for fit statistics and
compare to values in the observed data.

## Usage

``` r
fit_ppmc(
  x,
  ...,
  model_fit = NULL,
  item_fit = NULL,
  ndraws = NULL,
  probs = c(0.025, 0.975),
  return_draws = 0,
  force = FALSE
)
```

## Arguments

- x:

  An estimated model object (e.g., from
  [`dcm_estimate()`](https://measr.info/dev/reference/dcm_estimate.md)).

- ...:

  Unused. For future extensions.

- model_fit:

  The posterior predictive model checks to compute for an evaluation of
  model-level fit. If `NULL`, no model-level checks are computed. See
  details.

- item_fit:

  The posterior predictive model checks to compute for an evaluation of
  item-level fit. If `NULL`, no item-level checks are computed. See
  details.

- ndraws:

  The number of posterior draws to base the checks on. Must be less than
  or equal to the total number of posterior draws retained in the
  estimated model. If `NULL` (the default) the total number from the
  estimated model is used.

- probs:

  The percentiles to be computed by the
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) function
  for summarizing the posterior distribution for each fit statistic.

- return_draws:

  Number of posterior draws for each specified fit statistic to be
  returned. This does not affect the calculation of the posterior
  predictive checks, but can be useful for visualizing the fit
  statistics. Must be less than `ndraws` (or the total number of draws
  if `ndraws = NULL`). If `0` (the default), only summaries of the
  posterior are returned (no individual samples).

- force:

  If all requested PPMCs have already been added to the model object
  using
  [`add_fit()`](https://measr.info/dev/reference/model_evaluation.md),
  should they be recalculated. Default is `FALSE`.

## Value

A list with two elements, "model_fit" and "item_fit". If either
`model_fit = NULL` or `item_fit = NULL` in the function call, this will
be a one-element list, with the null criteria excluded. Each list
element, is itself a list with one element for each specified PPMC
containing a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html).
For example if `item_fit = c("conditional_prob", "odds_ratio")`, the
"item_fit" element will be a list of length two, where each element is a
tibble containing the results of the PPMC. All tibbles follow the same
general structure:

- `obs_{ppmc}`: The value of the relevant statistic in the observed
  data.

- `ppmc_mean`: The mean of the `ndraws` posterior samples calculated for
  the given statistic.

- Quantile columns: 1 column for each value of `probs`, providing the
  corresponding quantiles of the `ndraws` posterior samples calculated
  for the given statistic.

- `samples`: A list column, where each element contains a vector of
  length `return_draws`, representing samples from the posterior
  distribution of the calculated statistic. This column is excluded if
  `return_draws = 0`.

- `ppp`: The posterior predictive p-value. This is the proportion of
  posterior samples for calculated statistic that are greater than the
  observed value. Values very close to 0 or 1 indicate incompatibility
  between the fitted model and the observed data.

## Details

Posterior predictive model checks (PPMCs) use the posterior distribution
of an estimated model to compute different statistics. This creates an
expected distribution of the given statistic, *if our estimated
parameters are correct*. We then compute the statistic in our observed
data and compare the observed value to the expected distribution.
Observed values that fall outside of the expected distributions indicate
incompatibility between the estimated model and the observed data.

For DCMs, we currently support PPMCs at the model and item level. At the
model level, we calculate the expected raw score distribution
(`model_fit = "raw_score"`) as described by Thompson (2019) and Park et
al. (2015). At the item level, we can calculate the conditional
probability that a respondent in each class provides a correct response
(`item_fit = "conditional_prob"`) as described by Thompson (2019) and
Sinharay & Almond (2007) or the overall proportion correct for an item
(`item_fit = "pvalue"`), as described by Thompson (2019). We can also
calculate the odds ratio for each pair of items
(`item_fit = "odds_ratio"`) as described by Park et al. (2015) and
Sinharay et al. (2006).

## References

Park, J. Y., Johnson, M. S., Lee, Y-S. (2015). Posterior predictive
model checks for cognitive diagnostic models. *International Journal of
Quantitative Research in Education, 2*(3-4), 244-264.
[doi:10.1504/IJQRE.2015.071738](https://doi.org/10.1504/IJQRE.2015.071738)

Sinharay, S., & Almond, R. G. (2007). Assessing fit of cognitive
diagnostic models. *Educational and Psychological Measurement, 67*(2),
239-257.
[doi:10.1177/0013164406292025](https://doi.org/10.1177/0013164406292025)

Sinharay, S., Johnson, M. S., & Stern, H. S. (2006). Posterior
predictive assessment of item response theory models. *Applied
Psychological Measurement, 30*(4), 298-321.
[doi:10.1177/0146621605285517](https://doi.org/10.1177/0146621605285517)

Thompson, W. J. (2019). *Bayesian psychometrics for diagnostic
assessments: A proof of concept* (Research Report No. 19-01). University
of Kansas; Accessible Teaching, Learning, and Assessment Systems.
[doi:10.35542/osf.io/jzqs8](https://doi.org/10.35542/osf.io/jzqs8)

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

fit_ppmc(mdm_dina, model_fit = "raw_score")
#> $ppmc_raw_score
#> # A tibble: 1 × 5
#>   obs_chisq ppmc_mean `2.5%` `97.5%`   ppp
#>       <dbl>     <dbl>  <dbl>   <dbl> <dbl>
#> 1      5.80      6.15  0.974    15.8 0.405
#> 
```
