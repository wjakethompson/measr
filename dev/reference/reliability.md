# Estimate the reliability of a diagnostic classification model

For diagnostic classification models, reliability can be estimated at
the pattern or attribute level. Pattern-level reliability represents the
classification consistency and accuracy of placing students into an
overall mastery profile. Rather than an overall profile, attributes can
also be scored individually. In this case, classification consistency
and accuracy should be evaluated for each individual attribute, rather
than the overall profile. This is referred to as the *maximum a
posteriori* (MAP) reliability. Finally, it may be desirable to report
results as the probability of proficiency or mastery on each attribute
instead of a proficient/not proficient classification. In this case, the
reliability of the posterior probability should be reported. This is the
*expected a posteriori* (EAP) reliability.

## Usage

``` r
reliability(x, ..., threshold = 0.5, force = FALSE)
```

## Arguments

- x:

  The estimated model to be evaluated.

- ...:

  Unused. For future extensions.

- threshold:

  For `map_reliability`, the threshold applied to the attribute-level
  probabilities for determining the binary attribute classifications.
  Should be a numeric vector of length 1 (the same threshold is applied
  to all attributes), or length equal to the number of attributes. If a
  named vector is supplied, names should match the attribute names in
  the Q-matrix used to estimate the model. If unnamed, thresholds should
  be in the order the attributes were defined in the Q-matrix.

- force:

  If reliability information has already been added to the model object
  with
  [`add_reliability()`](https://measr.info/dev/reference/model_evaluation.md),
  should it be recalculated. Default is `FALSE`.

## Value

For class `measrdcm`, a list with 3 elements:

- `pattern_reliability`: The pattern-level accuracy (`p_a`) and
  consistency (`p_c`) described by Cui et al. (2012).

- `map_reliability`: A list with 2 elements: `accuracy` and
  `consistency`, which include the attribute-level classification
  reliability statistics described by Johnson & Sinharay (2018).

- `eap_reliability`: The attribute-level posterior probability
  reliability statistics described by Johnson & Sinharay (2020).

## Details

The pattern-level reliability (`pattern_reliability`) statistics are
described in Cui et al. (2012). Attribute-level classification
reliability statistics (`map_reliability`) are described in Johnson &
Sinharay (2018). Reliability statistics for the posterior mean of the
skill indicators (i.e., the mastery or proficiency probabilities;
`eap_reliability`) are described in Johnson & Sinharay (2019).

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

## Examples

``` r
rstn_mdm_lcdm <- dcm_estimate(
  dcm_specify(dcmdata::mdm_qmatrix, identifier = "item"),
  data = dcmdata::mdm_data, missing = NA, identifier = "respondent",
  method = "optim", seed = 63277, backend = "rstan"
)

reliability(rstn_mdm_lcdm)
#> $pattern_reliability
#>       p_a       p_c 
#> 0.9122250 0.8401031 
#> 
#> $map_reliability
#> $map_reliability$accuracy
#> # A tibble: 1 × 8
#>   attribute        acc lambda_a kappa_a youden_a tetra_a  tp_a  tn_a
#>   <chr>          <dbl>    <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>
#> 1 multiplication 0.912    0.820   0.823    0.824   0.962 0.923 0.901
#> 
#> $map_reliability$consistency
#> # A tibble: 1 × 10
#>   attribute   consist lambda_c kappa_c youden_c tetra_c  tp_c  tn_c gammak
#>   <chr>         <dbl>    <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>  <dbl>
#> 1 multiplica…   0.840    0.666   0.821    0.680   0.876 0.847 0.833  0.870
#> # ℹ 1 more variable: pc_prime <dbl>
#> 
#> 
#> $eap_reliability
#> # A tibble: 1 × 5
#>   attribute      rho_pf rho_bs rho_i rho_tb
#>   <chr>           <dbl>  <dbl> <dbl>  <dbl>
#> 1 multiplication  0.740  0.740 0.613  0.918
#> 
```
