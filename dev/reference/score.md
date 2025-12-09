# Posterior draws of respondent proficiency

Calculate posterior draws of respondent proficiency. Optionally retain
all posterior draws or return only summaries of the distribution for
each respondent.

## Usage

``` r
score(
  x,
  newdata = NULL,
  missing = NA,
  identifier = NULL,
  summary = TRUE,
  probs = c(0.025, 0.975),
  force = FALSE
)
```

## Arguments

- x:

  An estimated model (e.g., from
  [`dcm_estimate()`](https://measr.info/dev/reference/dcm_estimate.md).

- newdata:

  Optional new data. If not provided, the data used to estimate the
  model is scored. If provided, `newdata` should be a data frame with 1
  row per respondent and 1 column per item. All items that appear in
  `newdata` should appear in the data used to estimate `x`.

- missing:

  An `R` expression specifying how missing data in `data` is coded
  (e.g., `NA`, `"."`, `-99`, etc.). The default is `NA`.

- identifier:

  Optional. Variable name of a column in `newdata` that contains
  respondent identifiers. `NULL` (the default) indicates that no
  identifiers are present in the data, and row numbers will be used as
  identifiers. If `newdata` is not specified and the data used to
  estimate the model is scored, the `resp_id` is taken from the original
  data.

- summary:

  Should summary statistics be returned instead of the raw posterior
  draws? Only relevant if the model was estimated with
  `method = "mcmc"`. Default is `FALSE`.

- probs:

  The percentiles to be computed by the
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) function.
  Only relevant if the model was estimated with `method = "mcmc"`. Only
  used if `summary` is `TRUE`.

- force:

  If respondent estimates have already been added to the model object
  with
  [`add_respondent_estimates()`](https://measr.info/dev/reference/model_evaluation.md),
  should they be recalculated. Default is `FALSE`.

## Value

A list with two elements: `class_probabilities` and
`attribute_probabilities`.

If summary is `FALSE`, each element is a tibble with one row per
respondent. The columns include the respondent identifier, and one
column of probabilities for each of the possible classes or attributes
(as
[`posterior::rvar()`](https://mc-stan.org/posterior/reference/rvar.html)
objects).

If summary is `TRUE`, each element is a tibble with one row per
respondent and class or attribute. The columns include the respondent
identifier, `class` or `attribute`, `mean`, and one column for every
value specified in `probs`.
