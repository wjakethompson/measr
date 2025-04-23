# Define classes for estimation methods ----------------------------------------
stanmethod <- S7::new_class("stanmethod", package = "measr")

mcmc <- S7::new_class("mcmc", parent = stanmethod, package = "measr")
optim <- S7::new_class("optim", parent = stanmethod, package = "measr")
gqs <- S7::new_class("gqs", parent = stanmethod, package = "measr")


# Define classes for backends --------------------------------------------------
stanbackend <- S7::new_class("stanbackend", package = "measr")

rstan <- S7::new_class("rstan", parent = stanbackend, package = "measr")
cmdstanr <- S7::new_class("cmdstanr", parent = stanbackend, package = "measr")
