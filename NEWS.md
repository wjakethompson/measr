# measr 0.3.1

* Added a `NEWS.md` file to track changes to the package.

## New features

* Support for additional model specifications has been added (#10):
  * The compensatory reparameterized unified model (C-RUM) can now be estimated by defining `type = "crum"` in the `measr_dcm()` function.
  * Users can now drop higher order interactions from the loglinear cognitive diagnostic model (LCDM). A new argument for `measr_dcm()`, `max_interaction`, defines the highest order interactions to estimate. For example, `max_interaction = 2` will estimate only intercepts, main effects, and two-way interactions.
  * A new argument to `measr_dcm()`, `attribute_structure` allows users to specified either "unconstrained" relationships between attributes or "independent" attributes.

* Updated prior specifications:
  * Users can now specify a prior distribution for the structural parameters that govern the base rates of class membership (#2).
  * Safeguards were added to warn users when a specified prior is not defined for the chosen DCM sub-type. For example, an error is generated if a prior is defined for a slipping parameter, but the LCDM was chosen as the type of model to be estimated (#1).

## Minor improvements and fixes

* Fixed bug with `backend = "rstan"` where warmup iterations could be more than the total iterations requested by the user if warmup iterations were not also specified (#6).

* Additional specifications were added to `measr_extract()` for extracting results from an estimated model.
