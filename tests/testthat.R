library(testthat)
library(measr)

test_check("measr",
           filter = "[^ecpe|mcmc]")
