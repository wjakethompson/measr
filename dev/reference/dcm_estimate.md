# Fit Bayesian diagnostic classification models

Estimate diagnostic classification models (DCMs; also known as cognitive
diagnostic models) using 'Stan'. Models can be estimated using Stan's
optimizer, or full Markov chain Monte Carlo (MCMC).

## Usage

``` r
dcm_estimate(
  dcm_spec,
  data,
  missing = NA,
  identifier = NULL,
  method = c("mcmc", "optim"),
  backend = getOption("measr.backend", "rstan"),
  file = NULL,
  file_refit = getOption("measr.file_refit", "never"),
  ...
)
```

## Arguments

- dcm_spec:

  A DCM specification created with
  [`dcm_specify()`](https://dcmstan.r-dcm.org/reference/dcm_specify.html).

- data:

  Response data. A data frame with 1 row per respondent and 1 column per
  item.

- missing:

  An `R` expression specifying how missing data in `data` is coded
  (e.g., `NA`, `"."`, `-99`, etc.). The default is `NA`.

- identifier:

  Optional. Variable name of a column in `data` that contains respondent
  identifiers. `NULL` (the default) indicates that no identifiers are
  present in the data, and row numbers will be used as identifiers.

- method:

  Estimation method. Options are `"mcmc"`, which uses Stan's sampling
  method, or `"optim"`, which uses Stan's optimizer.

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

  - For `"on_change"`, the model will be refit if the `dcm_spec`,
    `data`, `method`, or `backend` specified are different from that in
    the saved `file`.

- ...:

  Additional arguments passed to Stan.

  - For `backend = "rstan"`, arguments are passed to
    [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
    or
    [`rstan::optimizing()`](https://mc-stan.org/rstan/reference/stanmodel-method-optimizing.html).

  - For `backend = "cmdstanr"`, arguments are passed to the
    [`$sample()`](https://mc-stan.org/cmdstanr/reference/model-method-sample.html)
    or
    [`$optimize()`](https://mc-stan.org/cmdstanr/reference/model-method-optimize.html)
    methods of the
    [CmdStanModel](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html)
    class.

## Value

A `measrdcm` object.

## Examples

``` r
model_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
                          identifier = "item")
model <- dcm_estimate(dcm_spec = model_spec, data = dcmdata::mdm_data,
                      identifier = "respondent", method = "optim",
                      seed = 63277)
```
