# Q-matrix validation

Calculate Q-matrix validation metrics for a fitted model objects using
methods described by de la Torre and Chiu (2016). See details for
additional information.

## Usage

``` r
qmatrix_validation(x, ..., pvaf_threshold = 0.95)
```

## Arguments

- x:

  A [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) object.

- ...:

  Unused.

- pvaf_threshold:

  The threshold for proportion of variance accounted for to flag items
  for appropriate empirical specifications. The default is .95 as
  implemented by de la Torre and Chiu (2016).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
containing the Q-matrix validation results. There is one row per item
with 5 columns:

- The item identifier, as specified in the Q-matrix and used to estimate
  the model.

- `original_specification`: The original Q-matrix entry for the item.

- `original_pvaf`: The proportion of variance accounted for by the
  original specification, compared to a specification where the item
  measures all attributes.

- `empirical_specification`: The Q-matrix specification that measures
  the fewest attributes with a proportion of variance accounted for over
  the the specified `pvaf_threshold` threshold. If the original
  specification is optimal, `empirical_specification` will be `NA`.

- `empirical_pvaf`: The proportion of variance accounted for by the
  empirical specification, compared to a specification where the item
  measures all attributes. If the original specification is optimal,
  `emprirical_pvaf` will be `NA`.

## Details

Q-matrix validation is conducted by evaluating the proportion of
variance accounted for by different Q-matrix specifications. Following
the method described by de la Torre and Chiu (2016), we use the
following steps for each item:

1.  Calculate the total variance explained if an item measured all
    possible attributes.

2.  For each possible Q-matrix entry, calculate the variance explained
    if the item measured the given attributes. Calculate the proportion
    of variance explained (PVAF) as the variance explained by the
    current Q-matrix entry divided by the variance explained by the
    saturated entry (Step 1).

3.  After computing the PVAF for all possible Q-matrix entries, filter
    to only those with a PVAF greater than the specified
    `pvaf_threshold` threshold.

4.  Filter the remaining Q-matrix entries to those that measure the
    fewest number of attributes (i.e., we prefer a more parsimonious
    model).

5.  If there is more than one Q-matrix entry remaining, select the entry
    with the highest PVAF.

## References

de la Torre, J., & Chiu, C.-Y. (2016). A general method of empirical
Q-matrix validation. *Psychometrika, 81*(2), 253-273.
[doi:10.1007/s11336-015-9467-8](https://doi.org/10.1007/s11336-015-9467-8)

## Examples

``` r
mod_spec <- dcm_specify(
  qmatrix = dcmdata::ecpe_qmatrix,
  identifier = "item_id",
  measurement_model = dcmstan::lcdm(),
  structural_model = dcmstan::hdcm(
    hierarchy = "lexical -> cohesive -> morphosyntactic"
  )
)
rstn_ecpe <- dcm_estimate(
  mod_spec,
  data = dcmdata::ecpe_data,
  identifier = "resp_id",
  backend = "rstan",
  method = "optim"
)

q_matrix_validation <- qmatrix_validation(rstn_ecpe)
```
