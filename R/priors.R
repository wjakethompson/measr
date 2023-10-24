#' Prior definitions for **measr** models
#'
#' Create prior definitions for classes of parameters, or specific parameters.
#'
#' @param prior A character string defining a distribution in **Stan** language.
#'   A list of all distributions supported by **Stan** can be found in *Stan
#'   Language Functions Reference* at
#'   \url{https://mc-stan.org/users/documentation/}.
#' @param class The parameter class. Defaults to `"intercept"`. Must be one of
#'   `"intercept"`, `"maineffect"`, `"interaction"` for the LCDM, or one of
#'   `"slip"` or `"guess"` for DINA or DINO models.
#' @param coef Name of a specific parameter within the defined class. If not
#'   defined, the prior is applied to all parameters within the class.
#' @param lb Lower bound for parameter restriction. Defaults to no restriction.
#' @param ub Upper bound for parameter restriction. Defaults to no restriction.
#' @param ... Additional arguments passed to `measrprior()`.
#'
#' @return A [tibble][tibble::tibble-package] of class `measrprior`.
#' @export
#'
#' @examples
#' # Use alias functions to define priors without quotes, as formulas,
#' # or as character strings.
#' (prior1 <- prior(lognormal(0, 1), class = maineffect))
#'
#' (prior2 <- prior_(~lognormal(0, 1), class = ~maineffect))
#'
#' (prior3 <- prior_string("lognormal(0, 1)", class = "maineffect"))
#'
#' identical(prior1, prior2)
#' identical(prior1, prior3)
#' identical(prior2, prior3)
#'
#' # Define a prior for an entire class of parameters
#' prior(beta(5, 25), class = "slip")
#'
#' # Or for a specific item (e.g., just the slipping parameter for item 7)
#' prior(beta(5, 25), class = "slip", coef = "slip[7]")
measrprior <- function(prior,
                       class = c("structural", "intercept", "maineffect",
                                 "interaction", "slip", "guess"),
                       coef = NA, lb = NA, ub = NA) {
  prior <- check_character(prior, allow_na = FALSE, name = "prior")
  class <- rlang::arg_match(class)
  coef <- check_character(coef, allow_na = TRUE, name = "coef")
  lb <- check_character(lb, allow_na = TRUE, name = "lb")
  ub <- check_character(ub, allow_na = TRUE, name = "ub")

  prior_spec <- tibble::tibble(prior = prior,
                 class = class,
                 coef = coef,
                 lb = lb,
                 ub = ub) %>%
    dplyr::mutate(
      coef = as.character(coef),
      bounds = dplyr::case_when(
        !is.na(.data$lb) | !is.na(.data$ub) ~
          glue::glue("T[{lb},{ub}]", .na = ""),
        TRUE ~ glue::glue("")
      ),
      prior_def = glue::glue("{prior}{bounds}"),
      prior_def = as.character(.data$prior_def)
    ) %>%
    dplyr::select("class", "coef", "prior_def")

  validate_measrprior(new_measrprior(prior_spec))
}


#' @describeIn measrprior Alias of `measrprior()` which allows arguments to be
#'   specified as expressions without quotation marks.
#' @export
prior <- function(prior, ...) {
  call <- as.list(match.call()[-1])
  call <- lapply(call, deparse_no_string)
  do.call(measrprior, call)
}

#' @describeIn measrprior Alias of `measrprior()` which allows arguments to be
#'   specified as one-sided formulas or wrapped in [base::quote()].
#' @export
prior_ <- function(prior, ...) {
  call <- named_list(prior, ...)
  call <- lapply(call, as_string)
  do.call(measrprior, call)
}

#' @describeIn measrprior Alias of `measrprior()` which allows arguments to be
#'   specified as character strings.
#' @export
prior_string <- function(prior, ...) {
  measrprior(prior, ...)
}

#' Default priors for diagnostic classification models
#'
#' @inheritParams measr_dcm
#'
#' @return A `measrprior` object.
#' @export
#'
#' @examples
#' default_dcm_priors(type = "lcdm")
default_dcm_priors <- function(type = "lcdm",
                               attribute_structure = "unconstrained") {
  type <- rlang::arg_match(type, dcm_choices())
  attribute_structure <- rlang::arg_match(attribute_structure, strc_choices())

  meas_prior <- if (type %in% c("lcdm", "crum")) {
    c(prior_string("normal(0, 2)", class = "intercept"),
      prior_string("lognormal(0, 1)", class = "maineffect"),
      if (type == "lcdm") prior_string("normal(0, 2)", class = "interaction"))
  } else if (type %in% c("dina", "dino")) {
    c(prior_string("beta(5, 25)", class = "slip"),
      prior_string("beta(5, 25)", class = "guess"))
  }

  strc_prior <- if (attribute_structure == "unconstrained") {
    prior_string("dirichlet(rep_vector(1, C))",
                 class = "structural", coef = "Vc")
  } else if (attribute_structure == "independent") {
    prior_string("beta(1, 1)", class = "structural")
  }

  prior <- c(meas_prior, strc_prior)
  return(prior)
}


#' Constructor for `measrprior` class
#'
#' @param x A data frame to be converted to a `measrprior` object.
#'
#' @noRd
new_measrprior <- function(x = data.frame()) {
  stopifnot(is.data.frame(x))
  stopifnot(all(sapply(x, is.character)))

  class(x) <- c("measrprior", class(x))
  x
}

#' Validator for `measrprior` class
#'
#' @param x An object of class `measrprior` to be checked for fidelity.
#'
#' @noRd
validate_measrprior <- function(x) {
  col_types <- sapply(x, typeof)
  needed_vars <- c("class", "coef", "prior_def")

  if (!all(needed_vars %in% names(col_types))) {
    abort_bad_argument(
      "x",
      must = "contain `class`, `coef`, and `prior_def` fields",
      extra = glue::glue(
        "Missing variables: ",
        "{paste(needed_vars[!needed_vars %in% names(col_types)],
                collapse = ', ')}"
      )
    )
  }

  if (!all(names(col_types) %in% needed_vars)) {
    abort_bad_argument(
      "x",
      must = "only contain `class`, `coef`, and `prior_def`",
      extra = glue::glue(
        "Extra variables: ",
        "{paste(names(col_types)[!names(col_types) %in% needed_vars],
                collapse = ', ')}"
      )
    )
  }

  if (any(c(is.na(x$class), is.na(x$prior_def)))) {
    abort_bad_argument("x",
                       must = glue::glue("not contain missing values for ",
                                         "`class` or `prior_def`"))
  }

  if (!all(x$class %in% c("structural",
                          "intercept", "maineffect", "interaction",
                          "slip", "guess"))) {
    abort_bad_argument("x",
                       must = glue::glue("only include values of ",
                                         "`intercept`, `maineffect`, and ",
                                         "`interaction` for prior class"))
  }

  x
}

#' Checks if argument is a `measrprior` object
#'
#' @param x An object
#'
#' @return A logical indicating if `x` is a `measrprior` object.
#'
#' @export
#' @examples
#' prior1 <- prior(lognormal(0, 1), class = maineffect)
#' is.measrprior(prior1)
#'
#' prior2 <- 3
#' is.measrprior(prior2)
is.measrprior <- function(x) { #nolint
  inherits(x, "measrprior")
}

#' Combine multiple measrprior objects into one measrprior
#'
#' @param x A `measrprior` object.
#' @param ... Additional `measrprior` objects to be combined.
#' @param replace Should only unique priors be kept? If `TRUE`, the first prior
#'   specified is kept.
#'
#' @return A `measrprior` object.
#'
#' @export
c.measrprior <- function(x, ..., replace = FALSE) {
  replace <- check_logical(replace, allow_na = FALSE, name = "replace")

  dots <- list(...)
  dots_class <- sapply(dots, is.measrprior)
  if (length(dots) && all(dots_class)) {
    out <- do.call(dplyr::bind_rows, list(x, ...))

    if (replace) {
      out <- dplyr::distinct(out, .data$class, .data$coef, .keep_all = TRUE)
    }
  } else {
    if (length(dots)) {
      bad_class <- class(dots[[which(!dots_class)[[1]]]])[[1]]
      abort_bad_argument(arg = "Objects",
                         must = "be `measrprior` objects",
                         not = bad_class)
    }
    out <- x
  }
  out
}
