# Maximum likelihood based information criteria

Calculate information criteria for diagnostic models not estimated with
full Markov chain Monte Carlo (i.e., with `method = "optim"`). Available
information include the Akaike information criterion (AIC; Akaike, 1973)
and the Bayesian information criterion (BIC; Schwarz, 1978).

## Usage

``` r
aic(x, ..., force = FALSE)

bic(x, ..., force = FALSE)
```

## Arguments

- x:

  A [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) object
  estimated with `backend = "optim"`.

- ...:

  Unused.

- force:

  If the criterion has already been added to the model object with
  [`add_criterion()`](https://measr.info/dev/reference/model_evaluation.md),
  should it be recalculated. Default is `FALSE`.

## Value

The numeric value of the information criterion.

## References

Akaike, H. (1973). Information theory and an extension of the maximum
likelihood principle. In B. N. Petrov & F. Csáki (Eds.), *Proceedings of
the Second International Symposium on Information Theory* (pp. 267-281).
Akademiai Kiado.

Schwarz, G. (1978). Estimating the dimension of a model. *The Annals of
Statistics, 6*(2), 461–464.
[doi:10.1214/aos/1176344136](https://doi.org/10.1214/aos/1176344136)

## Examples

``` r
model_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
                          identifier = "item")
model <- dcm_estimate(dcm_spec = model_spec, data = dcmdata::mdm_data,
                      identifier = "respondent", method = "optim",
                      seed = 63277)

aic(model)
#> [1] 707.0866

bic(model)
#> [1] 733.689
```
