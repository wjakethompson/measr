# S7 classes for estimation specifications

The constructors for Stan back-ends and methods are exported to support
extensions to measr, for example converting other models to `measrfit`
objects. We do not expect or recommend calling these functions directly
unless you are converting objects, or creating new methods for measrfit
objects.

## Usage

``` r
rstan()

cmdstanr()

mcmc()

optim()

gqs()
```

## Value

An [S7
object](https://rconsortium.github.io/S7/reference/S7_object.html) with
the corresponding class.

## Details

### Back-end classes

There are two classes for estimation backends, which define the package
that should be used, or was used, to estimate a model. The `rstan()`
class indicates use of `{rstan}`, whereas `cmdstanr()` indicates use of
`{cmdstanr}`. Both classes inherit from `measr::stanbackend`.

### Method classes

The method classes define which estimation method should be used, or was
used, for a model. The `mcmc()` class indicates the use of Markov chain
Monte Carlo via
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
when using `{rstan}` or the
[`$sample()`](https://mc-stan.org/cmdstanr/reference/model-method-sample.html)
method of the
[CmdStanModel](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html)
class when using `{cmdstanr}`. The `optim()` class indicates the use
maximum-likelihood via
[`rstan::optimizing()`](https://mc-stan.org/rstan/reference/stanmodel-method-optimizing.html)
when using `{rstan}` or the
[`$optimize()`](https://mc-stan.org/cmdstanr/reference/model-method-optimize.html)
method of the
[CmdStanModel](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html)
class when using `{cmdstanr}`. Finally, there is a `gqs()` class for use
when a model has previously been estimated and were are interested in
calculating generated quantities (e.g.,
[`score()`](https://measr.info/dev/reference/score.md),
[`loglik_array()`](https://measr.info/dev/reference/loglik_array.md)).
The `gqs()` class indicates the use of
[`rstan::gqs()`](https://mc-stan.org/rstan/reference/stanmodel-method-gqs.html)
when using `{rstan}` and the
[`$generate_quantities()`](https://mc-stan.org/cmdstanr/reference/model-method-generate-quantities.html)
method of the
[CmdStanModel](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html)
class when using `{cmdstanr}`. All method classes inherit from
`measr::stanmethod`.

## Examples

``` r
rstan()
#> <measr::rstan>

mcmc()
#> <measr::mcmc>
```
