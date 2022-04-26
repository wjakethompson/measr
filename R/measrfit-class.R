#' Class `measrfit` of models fitted with the **measr** package
#'
#' Something...
#'
#' @name measrfit-class
#' @aliases measrfit
#' @docType class
#'
#' @slot data
#' @slot prior A [measrprior][measrprior()] object containing information on the
#'   priors used in the model.
#' @slot stancode The model code in **Stan** language.
#' @slot algorithm
#' @slot backend
#' @slot model
#' @slot fit
#' @slot criteria
#' @slot reliability
#' @slot file
#' @slot version
#'
#' @seealso [measr_dcm()]
NULL



new_measrfit <- function(model = list()) {
  stopifnot(is.list(model))

  structure(model, class = "measrfit")
}

is_stanmodel <- function(x) {
  any(class(x) %in% c("stanfit", "CmdStanFit"))
}
