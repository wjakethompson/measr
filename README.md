
<!-- README.md is generated from README.Rmd. Please edit that file -->

# measr <img src="man/figures/logo.png" align ="right" width="120"/>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R package
version](https://www.r-pkg.org/badges/version/measr)](https://cran.r-project.org/package=measr)
[![Package
downloads](https://cranlogs.r-pkg.org/badges/grand-total/measr)](https://cran.r-project.org/package=measr)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.05742/status.svg)](https://doi.org/10.21105/joss.05742)</br>
[![R-CMD-check](https://github.com/wjakethompson/measr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wjakethompson/measr/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/wjakethompson/measr/branch/main/graph/badge.svg?token=JtF3xtGt6g)](https://app.codecov.io/gh/wjakethompson/measr)
[![Netlify
Status](https://api.netlify.com/api/v1/badges/b82caf01-0611-4f8b-bbca-5b89b5a80791/deploy-status)](https://app.netlify.com/sites/measr/deploys)</br>
[![Signed
by](https://img.shields.io/badge/Keybase-Verified-brightgreen.svg)](https://keybase.io/wjakethompson)
![License](https://img.shields.io/badge/License-GPL_v3-blue.svg)
<!-- badges: end -->

Diagnostic classification models (DCMs) are a class of psychometric
models that estimate respondent abilities as a profile of proficiency on
a pre-defined set of skills, or attributes. Despite the utility of DCMs
for providing fine-grained and actionable feedback with shorter
assessments, they have are not widely used in applied settings, in part
due to a lack of user-friendly software. Using
[R](https://www.r-project.org/) and [Stan](https://mc-stan.org/), measr
(said: “measure”) simplifies the process of estimating and evaluating
DCMs. Users can specify different DCM subtypes, define prior
distributions, and estimate the model using the
[rstan](https://mc-stan.org/rstan/) or
[cmdstanr](https://mc-stan.org/cmdstanr/) interface to Stan. You can
then easily examine model parameters, calculate model fit metrics,
compare competing models, and evaluate the reliability of the
attributes.

## Installation

You can install the released version of measr from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("measr")
```

To install the development version of measr from
[GitHub](https://github.com/wjakethompson/measr) use:

``` r
# install.packages("remotes")
remotes::install_github("wjakethompson/measr")
```

Because measr is based on Stan, a C++ compiler is required. For Windows,
the [Rtools program](https://cran.r-project.org/bin/windows/Rtools/)
comes with a C++ compiler. On Mac, it’s recommended that you install
Xcode. For additional instructions and help setting up the compilers,
see the [RStan installation help
page](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

## Usage

We can estimate a DCM using `measr_dcm()`. This function only requires a
data set of item responses and a Q-matrix defining which attributes are
measured by each item. We also identify any respondent or item
identifier columns. Other arguments can be specified to customize the
type of model to estimates (see `?measr_dcm()`).

To demonstrate measr’s functionality, example data sets are included.
Here we use the Examination of Certificate of Proficiency in English
(ECPE; [Templin & Hoffman, 2013](https://doi.org/10.1111/emip.12010))
data (see `?ecpe` for details). Note that by default, measr uses a full
Markov chain Monte Carlo (MCMC) estimation with Stan, which can be time
and computationally intensive. For a quicker estimation, we can use
Stan’s optimizer instead of MCMC by adding `method = "optim"` to the
function call. However, please not that some functionality will be lost
when using the optimizer (e.g., the calculation of relative fit criteria
requires the use of MCMC).

``` r
library(measr)

model <- dcm_estimate(
  dcm_spec = dcm_specify(dcmdata::ecpe_qmatrix, identifier = "item_id"),
  data = dcmdata::ecpe_data, identifier = "resp_id",
  iter = 1000, warmup = 200, chains = 2, cores = 2
)
```

Once a model has been estimated, we can then add and evaluate model fit.
This can done through absolute model fit, relative model fit
(information criteria), or reliability indices. Model parameters,
respondent classifications, and results of the model fit analyses can
then be extracted using `measr_extract()`.

``` r
model <- add_fit(model, method = "m2")
model <- add_criterion(model, criterion = "loo")
model <- add_reliability(model)

measr_extract(model, "m2")
#> # A tibble: 1 × 3
#>      m2    df     pval
#>   <dbl> <int>    <dbl>
#> 1  506.   325 4.37e-10
```

------------------------------------------------------------------------

Contributions are welcome. To ensure a smooth process, please review the
[Contributing Guide](https://measr.info/dev/CONTRIBUTING.html). Please
note that the measr project is released with a [Contributor Code of
Conduct](https://measr.info/CODE_OF_CONDUCT.html). By contributing to
this project, you agree to abide by its terms.
