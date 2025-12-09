# S7 class for measrdcm objects

The `measrdcm` constructor is exported to facilitate the conversion of
other model objects (e.g., `stanfit`) to `measrdcm` objects. We do not
expect or recommend calling this function directly, unless you are
creating a method for converting to `measrdcm`. Rather, to create a
`measrdcm` object, one should use
[`dcm_estimate()`](https://measr.info/dev/reference/dcm_estimate.md).

## Usage

``` r
measrdcm(
  model_spec = NULL,
  data = list(),
  stancode = character(0),
  method = stanmethod(),
  algorithm = character(0),
  backend = stanbackend(),
  model = list(),
  respondent_estimates = list(),
  fit = list(),
  criteria = list(),
  reliability = list(),
  file = character(0),
  version = list()
)
```

## Arguments

- model_spec:

  The model specification used to estimate the model.

- data:

  The data used to estimate the model.

- stancode:

  The model code in *Stan* language.

- method:

  The method used to fit the model.

- algorithm:

  The name of the algorithm used to fit the model.

- backend:

  The name of the backend used to fit the model.

- model:

  The fitted Stan model. This will object of class
  [rstan::stanfit](https://mc-stan.org/rstan/reference/stanfit-class.html)
  if `backend = "rstan"` and
  [`CmdStanMCMC`](https://mc-stan.org/cmdstanr/reference/CmdStanMCMC.html)
  if `backend = "cmdstanr"` was specified when fitting the model.

- respondent_estimates:

  An empty list for adding estimated person parameters after fitting the
  model.

- fit:

  An empty list for adding model fit information after fitting the
  model.

- criteria:

  An empty list for adding information criteria after fitting the model.

- reliability:

  An empty list for adding reliability information after fitting the
  model.

- file:

  Optional name of a file which the model objects was saved to or loaded
  from.

- version:

  The versions of measr, *Stan*, and rstan or cmdstanr that were used to
  fit the model.

## Value

A `measrdcm` object.

## See also

[`dcm_estimate()`](https://measr.info/dev/reference/dcm_estimate.md).

## Examples

``` r
qmatrix <- tibble::tibble(
  att1 = sample(0:1, size = 15, replace = TRUE),
  att2 = sample(0:1, size = 15, replace = TRUE),
  att3 = sample(0:1, size = 15, replace = TRUE),
  att4 = sample(0:1, size = 15, replace = TRUE)
)

spec <- dcm_specify(qmatrix = qmatrix)

measrdcm(spec)
#> <measr::measrdcm>
#>  @ model_spec          : <dcmstan::dcm_specification>
#>  .. @ qmatrix          : tibble [15 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ att1: int [1:15] 0 1 1 1 0 1 1 1 1 1 ...
#>  $ att2: int [1:15] 1 1 1 0 1 0 1 0 1 0 ...
#>  $ att3: int [1:15] 1 0 1 1 1 0 1 1 1 1 ...
#>  $ att4: int [1:15] 0 0 0 0 1 1 1 0 0 1 ...
#>  .. @ qmatrix_meta     :List of 3
#>  .. .. $ attribute_names: Named chr [1:4] "att1" "att2" "att3" "att4"
#>  .. ..  ..- attr(*, "names")= chr [1:4] "att1" "att2" "att3" "att4"
#>  .. .. $ item_identifier: NULL
#>  .. .. $ item_names     : Named int [1:15] 1 2 3 4 5 6 7 8 9 10 ...
#>  .. ..  ..- attr(*, "names")= chr [1:15] "1" "2" "3" "4" ...
#>  .. @ measurement_model: <dcmstan::LCDM>
#>  .. .. @ model     : chr "lcdm"
#>  .. .. @ model_args:List of 1
#>  .. .. .. $ max_interaction: num Inf
#>  .. @ structural_model : <dcmstan::UNCONSTRAINED>
#>  .. .. @ model     : chr "unconstrained"
#>  .. .. @ model_args: list()
#>  .. @ priors           : <dcmstan::dcmprior>
#>  .. .. @ distribution: chr [1:4] "normal(0, 2)" "lognormal(0, 1)" ...
#>  .. .. @ type        : chr [1:4] "intercept" "maineffect" "interaction" ...
#>  .. .. @ coefficient : chr [1:4] NA NA NA "Vc"
#>  .. .. @ lower_bound : num [1:4] NA NA NA NA
#>  .. .. @ upper_bound : num [1:4] NA NA NA NA
#>  .. .. @ prior       : chr [1:4] "normal(0, 2)" "lognormal(0, 1)" ...
#>  @ data                : list()
#>  @ stancode            : chr(0) 
#>  @ method              : <measr::stanmethod>
#>  @ algorithm           : chr(0) 
#>  @ backend             : <measr::stanbackend>
#>  @ model               : list()
#>  @ respondent_estimates: list()
#>  @ fit                 : list()
#>  @ criteria            : list()
#>  @ reliability         : list()
#>  @ file                : chr(0) 
#>  @ version             : list()
```
