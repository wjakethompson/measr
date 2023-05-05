# measr 0.2.1.9000

* Added a `NEWS.md` file to track changes to the package.

* Priors for the structural parameter simplex can now be defined using `prior()` (#2).

* Priors are now checked for compatibility with the specified model and Q-matrix (i.e., errors are thrown if the user specifies a prior for a class or parameter that is irrelevant to the defined model; #1).

* Fixed bug with `backend = "rstan"` where warmup iterations could be more than the total iterations requested by the user if warmup iterations were not also specified (#6).

* Added functionality for new model specifications. In `measr_dcm()`, the compensatory reparameterized unified model (C-RUM) can be estimated using `type = "crum"`. The LCDM can also be limited to a higher order interaction with the new `max_interaction` argument.

* Add new extract methods for PPMCs.
