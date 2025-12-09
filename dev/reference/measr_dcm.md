# Fit Bayesian diagnostic classification models

**\[deprecated\]**

`measr_dcm()` has been deprecated in favor of
[`dcm_estimate()`](https://measr.info/dev/reference/dcm_estimate.md).
Please use
[`dcm_estimate()`](https://measr.info/dev/reference/dcm_estimate.md), as
`measr_dcm()` will be removed in a future release.

## Usage

``` r
measr_dcm(
  data,
  missing = NA,
  qmatrix,
  resp_id = NULL,
  item_id = NULL,
  type = c("lcdm", "dina", "dino", "crum"),
  max_interaction = Inf,
  attribute_structure = c("unconstrained", "independent"),
  method = c("mcmc", "optim"),
  prior = NULL,
  backend = getOption("measr.backend", "rstan"),
  file = NULL,
  file_refit = getOption("measr.file_refit", "never"),
  ...
)
```

## Arguments

- data:

  Response data. A data frame with 1 row per respondent and 1 column per
  item.

- missing:

  An `R` expression specifying how missing data in `data` is coded
  (e.g., `NA`, `"."`, `-99`, etc.). The default is `NA`.

- qmatrix:

  The Q-matrix. A data frame with 1 row per item and 1 column per
  attribute. All cells should be either 0 (item does not measure the
  attribute) or 1 (item does measure the attribute).

- resp_id:

  Optional. Variable name of a column in `data` that contains respondent
  identifiers. `NULL` (the default) indicates that no identifiers are
  present in the data, and row numbers will be used as identifiers.

- item_id:

  Optional. Variable name of a column in `qmatrix` that contains item
  identifiers. `NULL` (the default) indicates that no identifiers are
  present in the Q-matrix. In this case, the column names of `data`
  (excluding any column specified in `resp_id`) will be used as the item
  identifiers. `NULL` also assumes that the order of the rows in the
  Q-matrix is the same as the order of the columns in `data` (i.e., the
  item in row 1 of `qmatrix` is the item in column 1 of `data`,
  excluding `resp_id`).

- type:

  Type of DCM to estimate. Must be one of `"lcdm"`, `"dina"`, `"dino"`,
  or `"crum"`.

- max_interaction:

  If `type = "lcdm"`, the highest level of interaction to estimate. The
  default is to estimate all possible interactions. For example, an item
  that measures 4 attributes would have 4 main effects, 6 two-way
  interactions, 4 three-way interactions, and 1 four-way interaction.
  Setting `max_interaction = 2` would result in only estimating the main
  effects and two-way interactions, excluding the three- and four- way
  interactions.

- attribute_structure:

  Structural model specification. Must be one of `"unconstrained"` or
  `"independent"`. `"unconstrained"` makes no assumptions about the
  relationships between attributes, whereas `"independent"` assumes that
  proficiency statuses on attributes are independent of each other.

- method:

  Estimation method. Options are `"mcmc"`, which uses Stan's sampling
  method, or `"optim"`, which uses Stan's optimizer.

- prior:

  A [prior](https://dcmstan.r-dcm.org/reference/prior.html) object. If
  `NULL`, default priors are used, as specified by
  [`dcmstan::default_dcm_priors()`](https://dcmstan.r-dcm.org/reference/default_dcm_priors.html).

- backend:

  Character string naming the package to use as the backend for fitting
  the Stan model. Options are `"rstan"` (the default) or `"cmdstanr"`.
  Can be set globally for the current `R` session via the
  "measr.backend" option (see
  [`options()`](https://rdrr.io/r/base/options.html)). Details on the
  rstan and cmdstanr packages are available at
  <https://mc-stan.org/rstan/> and <https://mc-stan.org/cmdstanr/>,
  respectively.

- file:

  Either `NULL` (the default) or a character string. If a character
  string, the fitted model object is saved as an `.rds` object using
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) using the supplied
  character string. The `.rds` extension is automatically added. If the
  specified file already exists, measr will load the previously saved
  model. Unless `file_refit` is specified, the model will not be refit.

- file_refit:

  Controls when a saved model is refit. Options are `"never"`,
  `"always"`, and `"on_change"`. Can be set globally for the current `R`
  session via the "measr.file_refit" option (see
  [`options()`](https://rdrr.io/r/base/options.html)).

  - For `"never"` (the default), the fitted model is always loaded if
    the `file` exists, and model fitting is skipped.

  - For `"always"`, the model is always refitted, regardless of whether
    or not `file` exists.

  - For `"on_change"`, the model will be refit if the `data`, `prior`,
    or `method` specified are different from that in the saved `file`.

- ...:

  Additional arguments passed to Stan.

  - For `backend = "rstan"`, arguments are passed to
    [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
    or
    [`rstan::optimizing()`](https://mc-stan.org/rstan/reference/stanmodel-method-optimizing.html).

  - For `backend = "cmdstanr"`, arguments are passed to the
    [sample](https://mc-stan.org/cmdstanr/reference/model-method-sample.html)
    or
    [optimize](https://mc-stan.org/cmdstanr/reference/model-method-optimize.html)
    methods of the
    [CmdStanModel](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html)
    class.

## Value

A `measrdcm` object.

## Examples

``` r
rstn_mdm_lcdm <- measr_dcm(
  data = mdm_data, missing = NA, qmatrix = mdm_qmatrix,
  resp_id = "respondent", item_id = "item", type = "lcdm",
  method = "optim", seed = 63277, backend = "rstan"
)
#> Warning: `measr_dcm()` was deprecated in measr 2.0.0.
#> ℹ This is a limited version of dcm_estimate(); use it instead.
#> Error: object 'mdm_qmatrix' not found
```
