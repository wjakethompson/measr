#' S7 classes for estimation specifications
#'
#' The constructors for Stan back-ends and methods are exported to support
#' extensions to measr, for example converting other models to `measrfit`
#' objects. We do not expect or recommend calling these functions directly
#' unless you are converting objects, or creating new methods for measrfit
#' objects.
#'
#' @details
#' ## Back-end classes
#'
#' There are two classes for estimation backends, which define the package that
#' should be used, or was used, to estimate a model.
#' The `rstan()` class indicates use of `{rstan}`, whereas `cmdstanr()`
#' indicates use of `{cmdstanr}`.
#' Both classes inherit from `measr::stanbackend`.
#'
#' ## Method classes
#'
#' The method classes define which estimation method should be used, or was
#' used, for a model.
#' The `mcmc()` class indicates the use of Markov chain Monte Carlo via
#' [rstan::sampling()] when using `{rstan}` or the
#' [`$sample()`][cmdstanr::sample()] method of the
#' [CmdStanModel][cmdstanr::CmdStanModel] class when using `{cmdstanr}`.
#' The `optim()` class indicates the use maximum-likelihood via
#' [rstan::optimizing()] when using `{rstan}` or the
#' [`$optimize()`][cmdstanr::optimize()] method of the
#' [CmdStanModel][cmdstanr::CmdStanModel] class when using `{cmdstanr}`.
#' Finally, there is a `gqs()` class for use when a model has previously been
#' estimated and were are interested in calculating generated quantities (e.g.,
#' [score()], [loglik_array()]).
#' The `gqs()` class indicates the use of [rstan::gqs()] when using `{rstan}`
#' and the [`$generate_quantities()`][cmdstanr::generate_quantities()] method of
#' the [CmdStanModel][cmdstanr::CmdStanModel] class when using `{cmdstanr}`.
#' All method classes inherit from `measr::stanmethod`.
#'
#' @return An [S7 object][S7::S7_object()] with the corresponding class.
#' @rdname stan-classes
#' @name stan-classes
#' @examples
#' rstan()
#'
#' mcmc()
NULL

# Define classes for backends --------------------------------------------------
stanbackend <- S7::new_class("stanbackend", package = "measr")

#' @export
#' @rdname stan-classes
rstan <- S7::new_class("rstan", parent = stanbackend, package = "measr")

#' @export
#' @rdname stan-classes
cmdstanr <- S7::new_class("cmdstanr", parent = stanbackend, package = "measr")

# Define classes for estimation methods ----------------------------------------
stanmethod <- S7::new_class("stanmethod", package = "measr")

#' @export
#' @rdname stan-classes
mcmc <- S7::new_class("mcmc", parent = stanmethod, package = "measr")

#' @export
#' @rdname stan-classes
optim <- S7::new_class("optim", parent = stanmethod, package = "measr")

#' @export
#' @rdname stan-classes
gqs <- S7::new_class("gqs", parent = stanmethod, package = "measr")
