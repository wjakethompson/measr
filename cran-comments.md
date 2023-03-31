## Release summary

This is a resubmission to address the following comments from the initial submission.

* References to methods have been added to the description field of the DESCRIPTION file.

* Missing \value Rd-tages have been added to all documentation pages.

## Test environments

* local R installation macOS 13.2.1, R 4.2.2
* macOS 12.6.3 (on github actions), R 4.2.3
* windows server 2022 (on github actions), R 4.2.3
* ubuntu 22.04.2 (on github actions), R 4.2.3
* ubuntu 22.04.2 (on github actions), R 4.1.3
* ubuntu 22.04.2 (on github actions), R-devel

## R CMD check results

0 errors | 0 warnings | 3 notes

* This is a new release.

* cmdstanr is a suggested package not on CRAN. Its availability is indicated in the Additional_repositories field in the DESCRIPTION. The cmdstanr package is completely optional, and is an alternative to rstan (also imported) for estimating models. The default behavior is to use rstan, but cmdstanr functionality is provided for users who may prefer that interface to the Stan language.

* RcppParallel and rstantools are declared imports are not used directly by this package, but are need for the configuration (e.g., `configure` and `configure.win`) and compiling the Stan models.

* GNU make is a system requirement for compiling the Stan models.
