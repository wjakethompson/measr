#' Generate mastery profiles
#'
#' Given the number of attributes, generate all possible patterns of attribute
#' mastery.
#'
#' @param attributes Positive integer. The number of attributes being measured.
#'
#' @return A [tibble][tibble::tibble-package] with all possible attribute
#'   mastery profiles. Each row is a profile, and each column indicates whether
#'   the attribute in that column was mastered (1) or not mastered (0). Thus,
#'   the tibble will have `2^attributes` rows, and `attributes` columns.
#' @export
#'
#' @examples
#' create_profiles(3L)
#' create_profiles(5)
create_profiles <- function(attributes) {
  attributes <- check_integer(attributes, name = "attributes")

  rep(list(c(0L, 1L)), times = attributes) %>%
    stats::setNames(glue::glue("att{seq_len(attributes)}")) %>%
    expand.grid() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::everything()))) %>%
    dplyr::select("total", dplyr::everything()) %>%
    dplyr::arrange(.data$total,
                   dplyr::desc(dplyr::across(dplyr::everything()))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"total") %>%
    tibble::as_tibble()
}


#' Evaluate an expression without printing output or messages
#'
#' @param expr expression to be evaluated
#' @param type type of output to be suppressed (see ?sink)
#' @param try wrap evaluation of expr in 'try' and
#'   not suppress outputs if evaluation fails?
#' @param silent actually evaluate silently?
#'
#' @noRd
eval_silent <- function(expr, type = "output", try = FALSE,
                        silent = TRUE, ...) {
  try <- as_one_logical(try)
  silent <- as_one_logical(silent)
  type <- match.arg(type, c("output", "message"))
  expr <- substitute(expr)
  envir <- parent.frame()
  if (silent) {
    if (try && type == "message") {
      try_out <- try(utils::capture.output(
        out <- eval(expr, envir), type = type, ...
      ))
      if (is(try_out, "try-error")) {
        # try again without suppressing error messages
        out <- eval(expr, envir)
      }
    } else {
      utils::capture.output(out <- eval(expr, envir), type = type, ...)
    }
  } else {
    out <- eval(expr, envir)
  }
  out
}

# coerce 'x' to a single logical value
as_one_logical <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.logical(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop2("Cannot coerce '", s, "' to a single logical value.")
  }
  x
}
