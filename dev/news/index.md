# Changelog

## measr (development version)

- Updated reliability functionality to allow for the calculation of
  accuracy and consistency with different thresholds for determining
  attribute classifications.

- Added new `measrfit()` function for creating measrfit objects from
  *Stan* models that were not originally created with measr.

- Added [`aic()`](https://measr.info/dev/reference/aic-bic.md) and
  [`bic()`](https://measr.info/dev/reference/aic-bic.md) functions for
  calculating the Akaike and Bayesian information criteria,
  respectively, for models estimated with `method = "optim"`.

- Refactored package to use S7 objects instead of S3.

- Functions for generating Stan code have been relocated to dcmstan.

- [`measr_dcm()`](https://measr.info/dev/reference/measr_dcm.md)
  deprecated in favor of
  [`dcm_estimate()`](https://measr.info/dev/reference/dcm_estimate.md).

- New functionality for relative model fit comparisons
  ([`aic()`](https://measr.info/dev/reference/aic-bic.md),
  [`bic()`](https://measr.info/dev/reference/aic-bic.md),
  [`bayes_factor()`](https://measr.info/dev/reference/bayes_factor.md)).

- New functionality for testing model assumptions
  ([`yens_q3()`](https://measr.info/dev/reference/yens_q3.md)).

## measr 1.0.0

CRAN release: 2024-01-30

### New documentation

- A new article on model evaluation has been added to the project
  website (<https://measr.info>).

- The model estimation article has been updated to use the same
  (simulated) data set as the model evaluation article.

- More detailed installation instructions have been added to the getting
  started vignette
  ([\#23](https://github.com/wjakethompson/measr/issues/23)).

- A case study demonstrating a full DCM-based analysis using data from
  the ECPE
  ([`?ecpe_data`](https://dcmdata.r-dcm.org/reference/ecpe.html)) has
  been added to the project website.

### Minor improvements and fixes

- Fixed bug in the LCDM specification of constraints for level-3 and
  above interaction terms.

- Functions for evaluating estimated models (e.g.,
  [`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md),
  [`reliability()`](https://measr.info/dev/reference/reliability.md)) no
  longer recalculate indices if they have previously been saved to the
  model object. This behavior can be overwritten with `force = TRUE`.

- Updated *Stan* syntax to be compatible with the new array syntax
  ([@andrjohns](https://github.com/andrjohns),
  [\#36](https://github.com/wjakethompson/measr/issues/36))

- [`get_parameters()`](https://dcmstan.r-dcm.org/reference/get_parameters.html)
  now preserves item identifiers by default. Items can be renamed with
  numbers (e.g., 1, 2, 3, …) by setting `rename_item = TRUE`.

- measr now reexports functions from
  [posterior](https://mc-stan.org/posterior/) for conducting
  mathematical operations on
  [`posterior::rvar()`](https://mc-stan.org/posterior/reference/rvar.html)
  objects.

- Respondent estimates are now returned as
  [`posterior::rvar()`](https://mc-stan.org/posterior/reference/rvar.html)
  objects when not summarized.

## measr 0.3.1

CRAN release: 2023-05-27

- Added a `NEWS.md` file to track changes to the package.

### New features

- Support for additional model specifications has been added
  ([\#10](https://github.com/wjakethompson/measr/issues/10)):
  - The compensatory reparameterized unified model (C-RUM) can now be
    estimated by defining `type = "crum"` in the
    [`measr_dcm()`](https://measr.info/dev/reference/measr_dcm.md)
    function.
  - Users can now drop higher order interactions from the loglinear
    cognitive diagnostic model (LCDM). A new argument for
    [`measr_dcm()`](https://measr.info/dev/reference/measr_dcm.md),
    `max_interaction`, defines the highest order interactions to
    estimate. For example, `max_interaction = 2` will estimate only
    intercepts, main effects, and two-way interactions.
  - A new argument to
    [`measr_dcm()`](https://measr.info/dev/reference/measr_dcm.md),
    `attribute_structure` allows users to specified either
    “unconstrained” relationships between attributes or “independent”
    attributes.
- Updated prior specifications:
  - Users can now specify a prior distribution for the structural
    parameters that govern the base rates of class membership
    ([\#2](https://github.com/wjakethompson/measr/issues/2)).
  - Safeguards were added to warn users when a specified prior is not
    defined for the chosen DCM sub-type. For example, an error is
    generated if a prior is defined for a slipping parameter, but the
    LCDM was chosen as the type of model to be estimated
    ([\#1](https://github.com/wjakethompson/measr/issues/1)).

### Minor improvements and fixes

- Fixed bug with `backend = "rstan"` where warmup iterations could be
  more than the total iterations requested by the user if warmup
  iterations were not also specified
  ([\#6](https://github.com/wjakethompson/measr/issues/6)).

- Additional specifications were added to
  [`measr_extract()`](https://measr.info/dev/reference/measr_extract.md)
  for extracting results from an estimated model.
