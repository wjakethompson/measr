deparse_no_string <- function(x) {
  if (!is.character(x)) {
    x <- deparse_combine(x)
  }
  x
}
deparse_combine <- function(x, max_char = NULL) {
  out <- paste(deparse(x), sep = "", collapse = "")
  if (isTRUE(max_char > 0)) {
    out <- substr(out, 1L, max_char)
  }
  out
}

as_string <- function(x) {
  if (inherits(x, "formula") && length(x) == 2) {
    deparse_no_string(x[[2]])
  } else if (is.call(x) || is.name(x) || is.atomic(x)) {
    deparse_no_string(x)
  } else {
    abort_bad_argument("Arguments",
                       must = "be one-sided formula, call, name, or constant.")
  }
}


#' Create a named list
#'
#' Creates a named list using the object names.
#'
#' @param ...
#'
#' @return A named list.
#' @noRd
named_list <- function(...) {
  m <- match.call()
  dots <- list(...)
  no_names <- is.null(names(dots))
  has_name <- if (no_names) FALSE else nzchar(names(dots))
  if (all(has_name)) return(dots)
  nms <- as.character(m)[-1]
  if (no_names) {
    names(dots) <- nms
  } else {
    names(dots)[!has_name] <- nms[!has_name]
  }
  dots
}
