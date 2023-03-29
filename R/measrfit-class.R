#' Class `measrfit` of models fitted with the **measr** package
#'
#' Models fitted with the **measr** package are represented as a `measrfit`
#' object, which contains the posterior draws, Stan code, priors, and other
#' relevant information.
#'
#' @name measrfit-class
#' @aliases measrfit
#' @docType class
#'
#' @slot data The data and Q-matrix used to estimate the model.
#' @slot type The type of DCM that was estimated.
#' @slot prior A [measrprior][measrprior()] object containing information on the
#'   priors used in the model.
#' @slot stancode The model code in **Stan** language.
#' @slot method The method used to fit the model.
#' @slot algorithm The name of the algorithm used to fit the model.
#' @slot backend The name of the backend used to fit the model.
#' @slot model The fitted Stan model. This will object of class
#'   [rstan::stanfit-class] if `backend = "rstan"` and
#'   [`CmdStanMCMC`](https://mc-stan.org/cmdstanr/reference/CmdStanMCMC.html)
#'   if `backend = "cmdstanr"` was specified when fitting the model.
#' @slot respondent_estimates An empty list for adding estimated person
#'   parameters after fitting the model.
#' @slot fit An empty list for adding model fit information after fitting the
#'   model.
#' @slot criteria An empty list for adding information criteria after fitting
#'   the model.
#' @slot reliability An empty list for adding reliability information after
#'   fitting the model.
#' @slot file Optional name of a file which the model objects was saved to
#'   or loaded from.
#' @slot version The versions of **measr**, **Stan**, **rstan** and/or
#'   **cmdstanr** that were used to fit the model.
#'
#' @seealso [measr_dcm()]
NULL


new_measrfit <- function(model = list(), ..., class = character()) {
  stopifnot(is.list(model))

  structure(model, ..., class = c(class, "measrfit"))
}

new_measrdcm <- function(x) {
  new_measrfit(x, class = "measrdcm")
}
