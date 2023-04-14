# measr 0.2.1.9000

* Added a `NEWS.md` file to track changes to the package.

* Priors for the structural parameter simplex can now be defined using `prior()` (#2).

* Priors are now checked for compatibility with the specified model and Q-matrix (i.e., errors are thrown if the user specifies a prior for a class or parameter that is irrelevant to the defined model; #1).
