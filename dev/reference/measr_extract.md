# Extract components of a `measrfit` object

Extract model metadata, parameter estimates, and model evaluation
results.

## Usage

``` r
measr_extract(model, what, ...)
```

## Arguments

- model:

  The estimated to extract information from.

- what:

  Character string. The information to be extracted. See details for
  available options.

- ...:

  Additional arguments passed to each extract method.

  - `ppmc_interval`:

    For `what = "odds_ratio_flags"` and
    `what = "conditional_prob_flags"`, the compatibility interval used
    for determining model fit flags to return. For example, a
    `ppmc_interval` of 0.95 (the default) will return any PPMCs where
    the posterior predictive *p*-value (ppp) is less than 0.025 or
    greater than 0.975.

  - `agreement`:

    For `what = "classification_reliability"`, additional measures of
    agreement to include. By default, the classification accuracy and
    consistency metrics defined Johnson & Sinharay (2018) are returned.
    Additional metrics that can be specified to `agreement` are Goodman
    & Kruskal's lambda (`lambda`), Cohen's kappa (`kappa`), Youden's
    statistic (`youden`), the tetrachoric correlation (`tetra`), true
    positive rate (`tp`), and the true negative rate (`tn`).

    For `what = "probability_reliability"`, additional measures of
    agreement to include. By default, the informational reliability
    index defined by Johnson & Sinharay (2020) is returned. Additional
    metrics that can be specified to `agreement` are the point biserial
    reliability index (`bs`), parallel forms reliability index (`pf`),
    and the tetrachoric reliability index (`tb`), which was originally
    defined by Templin & Bradshaw (2013).

## Value

The extracted information. The specific structure will vary depending on
what is being extracted, but usually the returned object is a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the requested information.

## Details

For diagnostic classification models, we can extract the following
information:

### Model metadata

- `prior`: The priors used when estimating the model.

- `classes`: The possible classes or profile patterns. This will show
  the class label (i.e., the pattern of proficiency) and the attributes
  included in each class.

### Estimated model components

#### Model parameters

- `item_param`: The estimated item parameters. This shows the name of
  the parameter, the class of the parameter, and the estimated value.

- `strc_param`: The estimated structural parameters. This is the base
  rate of membership in each class. This shows the class pattern and the
  estimated proportion of respondents in each class.

- `pi_matrix`: The model estimated probability that a respondent in the
  given class provides a correct response to the item. The output shows
  the the item (rows), class (columns), and estimated *p*-values.

- `exp_pvalues`: Model expected *p*-values for each item. This is
  equivalent to the `pi_matrix`, but also includes and "overall"
  variable, which represents the expected *p*-value for each item (i.e.,
  an average of the class-specific *p*-values, weighted by the
  prevalence of each class).

#### Respondent results

- `class_prob`: The probability that each respondent belongs to class
  (i.e., has the given pattern of proficiency).

- `attribute_prob`: The proficiency probability for each respondent and
  attribute.

### Model fit

#### Absolute model fit

- `m2`: The M₂ fit statistic. See
  [`fit_m2()`](https://rdrr.io/pkg/dcm2/man/fit_m2.html) for details.
  Model fit information must first be added to the model using
  [`add_fit()`](https://measr.info/dev/reference/model_evaluation.md).

- `rmsea`: The root mean square error of approximation (RMSEA) fit
  statistic and associated confidence interval. See
  [`fit_m2()`](https://rdrr.io/pkg/dcm2/man/fit_m2.html) for details.
  Model fit information must first be added to the model using
  [`add_fit()`](https://measr.info/dev/reference/model_evaluation.md).

- `srmsr`: The standardized root mean square residual (SRMSR) fit
  statistic. See [`fit_m2()`](https://rdrr.io/pkg/dcm2/man/fit_m2.html)
  for details. Model fit information must first be added to the model
  using
  [`add_fit()`](https://measr.info/dev/reference/model_evaluation.md).

- `ppmc_raw_score`: The observed and posterior predicted chi-square
  statistic for the raw score distribution. See
  [`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md) for
  details. Model fit information must first be added to the model using
  [`add_fit()`](https://measr.info/dev/reference/model_evaluation.md).

- `ppmc_conditional_prob`: The observed and posterior predicted
  conditional probabilities of each class providing a correct response
  to each item. See
  [`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md) for
  details. Model fit information must first be added to the model using
  [`add_fit()`](https://measr.info/dev/reference/model_evaluation.md).

- `ppmc_conditional_prob_flags`: A subset of the PPMC conditional
  probabilities where the *ppp* is outside the specified
  `ppmc_interval`.

- `ppmc_odds_ratio`: The observed and posterior predicted odds ratios of
  each item pair. See
  [`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md) for
  details. Model fit information must first be added to the model using
  [`add_fit()`](https://measr.info/dev/reference/model_evaluation.md).

- `ppmc_odds_ratio_flags`: A subset of the PPMC odds ratios where the
  *ppp* is outside the specified `ppmc_interval`.

- `ppmc_pvalue`: The observed and posterior predicted proportion of
  correct responses to each item. See
  [`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md) for
  details.

- `ppmc_pvalue_flags`: A subset of the PPMC proportion correct values
  where the *ppp* is outside the specified `ppmc_interval`.

#### Relative model fit

- `loo`: The leave-one-out cross validation results. See
  [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) for
  details. The information criterion must first be added to the model
  using
  [`add_criterion()`](https://measr.info/dev/reference/model_evaluation.md).

- `waic`: The widely applicable information criterion results. See
  [`loo::waic()`](https://mc-stan.org/loo/reference/waic.html) for
  details. The information criterion must first be added to the model
  using
  [`add_criterion()`](https://measr.info/dev/reference/model_evaluation.md).

- `aic`: The Akaike information criterion results. See
  [`aic()`](https://measr.info/dev/reference/aic-bic.md) for details.
  The information criterion must first be added to the model using
  [`add_criterion()`](https://measr.info/dev/reference/model_evaluation.md).

- `bic`: The Bayesian information criterion results. See
  [`bic()`](https://measr.info/dev/reference/aic-bic.md) for details.
  The information criterion must first be added to the model using
  [`add_criterion()`](https://measr.info/dev/reference/model_evaluation.md).

### Reliability

- `pattern_reliability`: The accuracy and consistency of the overall
  attribute profile classification, as described by Cui et al. (2012).
  Reliability information must first be added to the model using
  [`add_reliability()`](https://measr.info/dev/reference/model_evaluation.md).

- `classification_reliability`: The classification accuracy and
  consistency for each attribute, using the metrics described by Johnson
  & Sinharay (2018). Reliability information must first be added to the
  model using
  [`add_reliability()`](https://measr.info/dev/reference/model_evaluation.md).

- `probability_reliability`: Reliability estimates for the probability
  of proficiency on each attribute, as described by Johnson & Sinharay
  (2020). Reliability information must first be added to the model using
  [`add_reliability()`](https://measr.info/dev/reference/model_evaluation.md).

## References

Cui, Y., Gierl, M. J., & Chang, H.-H. (2012). Estimating classification
consistency and accuracy for cognitive diagnostic assessment. *Journal
of Educational Measurement, 49*(1), 19-38.
[doi:10.1111/j.1745-3984.2011.00158.x](https://doi.org/10.1111/j.1745-3984.2011.00158.x)

Johnson, M. S., & Sinharay, S. (2018). Measures of agreement to assess
attribute-level classification accuracy and consistency for cognitive
diagnostic assessments. *Journal of Educational Measurement, 55*(4),
635-664. [doi:10.1111/jedm.12196](https://doi.org/10.1111/jedm.12196)

Johnson, M. S., & Sinharay, S. (2020). The reliability of the posterior
probability of skill attainment in diagnostic classification models.
*Journal of Educational and Behavioral Statistics, 45*(1), 5-31.
[doi:10.3102/1076998619864550](https://doi.org/10.3102/1076998619864550)

Templin, J., & Bradshaw, L. (2013). Measuring the reliability of
diagnostic classification model examinee estimates. *Journal of
Classification, 30*(2), 251-275.
[doi:10.1007/s00357-013-9129-4](https://doi.org/10.1007/s00357-013-9129-4)

## Examples

``` r
rstn_mdm_lcdm <- dcm_estimate(
  dcm_specify(dcmdata::mdm_qmatrix, identifier = "item"),
  data = dcmdata::mdm_data, missing = NA, identifier = "respondent",
  method = "optim", seed = 63277, backend = "rstan"
)

measr_extract(rstn_mdm_lcdm, "strc_param")
#> # A tibble: 2 × 2
#>   class estimate
#>   <chr>    <dbl>
#> 1 [0]      0.488
#> 2 [1]      0.512
```
