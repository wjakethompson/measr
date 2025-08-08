#' Evaluate an expression without printing output or messages
#'
#' @param expr Expression to be evaluated.
#'
#' @noRd
eval_silent <- function(expr) {
  expr <- substitute(expr)
  envir <- parent.frame()

  try_out <- try(utils::capture.output(
    out <- eval(expr, envir),
    type = "message"
  ))
  if (methods::is(try_out, "try-error")) {
    # try again without suppressing error messages
    out <- eval(expr, envir)
  }
  out
}

#' Determine if code is executed interactively or in pkgdown
#'
#' Used for determining examples that shouldn't be run on CRAN, but can be run
#' for the pkgdown website.
#'
#' @return A logical value indicating whether or not the examples should be run.
#'
#' @export
#' @examples
#' measr_examples()
measr_examples <- function() {
  interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
}
