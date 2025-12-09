# Item, attribute, and test-level discrimination indices

The cognitive diagnostic index (CDI) is a measure of how well an
assessment is able to distinguish between attribute profiles. The index
was originally proposed by Henson & Douglas (2005) for item- and
test-level discrimination, and then expanded by Henson et al. (2008) to
include attribute-level discrimination indices.

## Usage

``` r
cdi(model, weight_prevalence = TRUE)
```

## Arguments

- model:

  The estimated model to be evaluated.

- weight_prevalence:

  Logical indicating whether the discrimination indices should be
  weighted by the prevalence of the attribute profiles. See details for
  additional information.

## Value

A list with two elements:

- `item_discrimination`: A
  [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  with one row per item containing the CDI for the item and any relevant
  attributes.

- `test_discrimination`: A
  [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  with one row containing the total CDI for the assessment and for each
  attribute.

## Details

Henson et al. (2008) described two attribute-level discrimination
indices, \\\mathbf{d}\_{(A)\mathbf{\cdot}}\\ (Equation 8) and
\\\mathbf{d}\_{(B)\mathbf{\cdot}}\\ (Equation 13), which are similar in
that both are the sum of item-level discrimination indices. In both
cases, item-level discrimination indices are calculated as the average
of Kullback-Leibler information for all pairs of attributes profiles for
the item. The item-level indices are then summed to achieve the
test-level discrimination index for each attribute, or the test overall.
However, whereas \\\mathbf{d}\_{(A)\mathbf{\cdot}}\\ is an unweighted
average of the Kullback-Leibler information,
\\\mathbf{d}\_{(B)\mathbf{\cdot}}\\ is a weighted average, where the
weight is defined by the prevalence of each profile (i.e.,
[`measr_extract(model, what = "strc_param")`](https://measr.info/dev/reference/measr_extract.md)).

## References

Henson, R., & Douglas, J. (2005). Test construction for cognitive
diagnosis. *Applied Psychological Measurement, 29*(4), 262-277.
[doi:10.1177/0146621604272623](https://doi.org/10.1177/0146621604272623)

Henson, R., Roussos, L., Douglas, J., & Xuming, H. (2008). Cognitive
diagnostic attribute-level discrimination indices. *Applied
Psychological Measurement, 32*(4), 275-288.
[doi:10.1177/0146621607302478](https://doi.org/10.1177/0146621607302478)

## Examples

``` r
rstn_ecpe_lcdm <- dcm_estimate(
  dcm_specify(dcmdata::ecpe_qmatrix, identifier = "item_id"),
  data = dcmdata::ecpe_data, missing = NA, identifier = "resp_id",
  method = "optim", seed = 63277, backend = "rstan"
)

cdi(rstn_ecpe_lcdm)
#> $item_discrimination
#> # A tibble: 28 × 5
#>     item overall morphosyntactic cohesive lexical
#>    <int>   <dbl>           <dbl>    <dbl>   <dbl>
#>  1     1  0.0615          0.0435   0.0618  0     
#>  2     2  0.0483          0        0.1000  0     
#>  3     3  0.0917          0.136    0       0.0462
#>  4     4  0.142           0        0       0.294 
#>  5     5  0.0929          0        0       0.192 
#>  6     6  0.0845          0        0       0.175 
#>  7     7  0.189           0.277    0       0.0756
#>  8     8  0.0660          0        0.137   0     
#>  9     9  0.0720          0        0       0.149 
#> 10    10  0.191           0.397    0       0     
#> # ℹ 18 more rows
#> 
#> $test_discrimination
#> # A tibble: 1 × 4
#>   overall morphosyntactic cohesive lexical
#>     <dbl>           <dbl>    <dbl>   <dbl>
#> 1    3.42            2.53    0.872    3.48
#> 
```
