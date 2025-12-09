# Add model evaluation metrics model objects

Add model evaluation metrics to fitted model objects. These functions
are wrappers around other functions that compute the metrics. The
benefit of using these wrappers is that the model evaluation metrics are
saved as part of the model object so that time-intensive calculations do
not need to be repeated. See Details for specifics.

## Usage

``` r
add_criterion(
  x,
  criterion = c("loo", "waic", "log_mll", "aic", "bic"),
  overwrite = FALSE,
  save = TRUE,
  ...,
  r_eff = NA
)

add_reliability(x, overwrite = FALSE, save = TRUE, ...)

add_fit(
  x,
  method = c("m2", "ppmc"),
  overwrite = FALSE,
  save = TRUE,
  ...,
  ci = 0.9
)

add_respondent_estimates(
  x,
  probs = c(0.025, 0.975),
  overwrite = FALSE,
  save = TRUE
)
```

## Arguments

- x:

  A [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) object.

- criterion:

  A vector of information criteria to calculate and add to the model
  object. Must be `"loo"`, `"waic"`, or `"log_mll"` for models estimated
  with MCMC, or `"aic"` or `"bic"` for models estimated with the
  optimizer.

- overwrite:

  Logical. Indicates whether specified elements that have already been
  added to the estimated model should be overwritten. Default is
  `FALSE`.

- save:

  Logical. Only relevant if a file was specified in the
  [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) object
  passed to `x`. If `TRUE` (the default), the model is re-saved to the
  specified file when new criteria are added to the `R` object. If
  `FALSE`, the new criteria will be added to the `R` object, but the
  saved file will not be updated.

- ...:

  Arguments passed on to
  [`fit_ppmc`](https://measr.info/dev/reference/fit_ppmc.md)

  `model_fit`

  :   The posterior predictive model checks to compute for an evaluation
      of model-level fit. If `NULL`, no model-level checks are computed.
      See details.

  `item_fit`

  :   The posterior predictive model checks to compute for an evaluation
      of item-level fit. If `NULL`, no item-level checks are computed.
      See details.

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

- method:

  A vector of model fit methods to evaluate and add to the model object.

- ci:

  The confidence interval for the RMSEA, computed from the M2

- probs:

  The percentiles to be computed by the
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) function.
  Only relevant if the model was estimated with `method = "mcmc"`. Only
  used if `summary` is `TRUE`.

## Value

A modified [measrdcm](https://measr.info/dev/reference/dcm_estimate.md)
object with the corresponding slot populated with the specified
information.

## Details

For `add_respondent_estimates()`, estimated person parameters are added
to the `$respondent_estimates` element of the fitted model.

For `add_fit()`, model and item fit information are added to the `$fit`
element of the fitted model. This function wraps
[`fit_m2()`](https://rdrr.io/pkg/dcm2/man/fit_m2.html) to calculate the
M₂ statistic (Hansen et al., 2016; Liu et al., 2016) and/or
[`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md) to
calculate posterior predictive model checks (Park et al., 2015; Sinharay
& Almond, 2007; Sinharay et al., 2006; Thompson, 2019), depending on
which methods are specified.

For `add_criterion()`, relative fit criteria are added to the
`$criteria` element of the fitted model. For models estimated with MCMC,
this function wraps
[`loo()`](https://mc-stan.org/loo/reference/loo.html) or
[`waic()`](https://mc-stan.org/loo/reference/waic.html) to calculate the
LOO-CV (Vehtari et al., 2017) or WAIC (Watanabe, 2010), respectively, or
[`log_mll()`](https://measr.info/dev/reference/log_mll.md) to calculate
the log marginal likelihood, which is used for calculating Bayes
factors. For models estimated with the optimizer, this wraps
[`aic()`](https://measr.info/dev/reference/aic-bic.md) or
[`bic()`](https://measr.info/dev/reference/aic-bic.md) to estimate the
AIC (Akaike, 1973) or BIC (Schwarz, 1978), respectively.

For `add_reliability()`, reliability information is added to the
`$reliability` element of the fitted model. Pattern level reliability is
described by Cui et al. (2012). Classification reliability and posterior
probability reliability are described by Johnson & Sinharay (2018,
2020), respectively. This function wraps
[`reliability()`](https://measr.info/dev/reference/reliability.md).
Arguments supplied to `...` are passed to
[`reliability()`](https://measr.info/dev/reference/reliability.md).

## References

Akaike, H. (1973). Information theory and an extension of the maximum
likelihood principle. In B. N. Petrov & F. Csáki (Eds.), *Proceedings of
the Second International Symposium on Information Theory* (pp. 267-281).
Akademiai Kiado.

Cui, Y., Gierl, M. J., & Chang, H.-H. (2012). Estimating classification
consistency and accuracy for cognitive diagnostic assessment. *Journal
of Educational Measurement, 49*(1), 19-38.
[doi:10.1111/j.1745-3984.2011.00158.x](https://doi.org/10.1111/j.1745-3984.2011.00158.x)

Hansen, M., Cai, L., Monroe, S., & Li, Z. (2016). Limited-information
goodness-of-fit testing of diagnostic classification item response
models. *British Journal of Mathematical and Statistical Psychology,
69*(3), 225-252.
[doi:10.1111/bmsp.12074](https://doi.org/10.1111/bmsp.12074)

Johnson, M. S., & Sinharay, S. (2018). Measures of agreement to assess
attribute-level classification accuracy and consistency for cognitive
diagnostic assessments. *Journal of Educational Measurement, 55*(4),
635-664. [doi:10.1111/jedm.12196](https://doi.org/10.1111/jedm.12196)

Johnson, M. S., & Sinharay, S. (2020). The reliability of the posterior
probability of skill attainment in diagnostic classification models.
*Journal of Educational and Behavioral Statistics, 45*(1), 5-31.
[doi:10.3102/1076998619864550](https://doi.org/10.3102/1076998619864550)

Liu, Y., Tian, W., & Xin, T. (2016). An application of M₂ statistic to
evaluate the fit of cognitive diagnostic models. *Journal of Educational
and Behavioral Statistics, 41*(1), 3-26.
[doi:10.3102/1076998615621293](https://doi.org/10.3102/1076998615621293)

Park, J. Y., Johnson, M. S., Lee, Y-S. (2015). Posterior predictive
model checks for cognitive diagnostic models. *International Journal of
Quantitative Research in Education, 2*(3-4), 244-264.
[doi:10.1504/IJQRE.2015.071738](https://doi.org/10.1504/IJQRE.2015.071738)

Schwarz, G. (1978). Estimating the dimension of a model. *The Annals of
Statistics, 6*(2), 461–464.
[doi:10.1214/aos/1176344136](https://doi.org/10.1214/aos/1176344136)

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

Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing, 27*(5), 1413-1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)

Watanabe, S. (2010). Asymptotic equivalence of Bayes cross validation
and widely applicable information criterion in singular learning theory.
*Journal of Machine Learning Research, 11*(116), 3571-3594.
<https://jmlr.org/papers/v11/watanabe10a.html>

## Examples

``` r
cmds_mdm_dina <- dcm_estimate(
  dcm_specify(dcmdata::mdm_qmatrix, identifier = "item",
              measurement_model = dina(),
              priors = c(prior(beta(5, 17), type = "slip"),
                         prior(beta(5, 17), type = "guess"))),
  data = dcmdata::mdm_data, missing = NA, identifier = "respondent",
  method = "optim", seed = 63277, backend = "rstan"
)

cmds_mdm_dina <- add_reliability(cmds_mdm_dina)
cmds_mdm_dina <- add_fit(cmds_mdm_dina, method = "m2")
cmds_mdm_dina <- add_respondent_estimates(cmds_mdm_dina)
```
