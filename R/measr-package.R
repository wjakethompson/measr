#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom tibble tibble
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @useDynLib measr, .registration = TRUE
## usethis namespace: end
NULL

release_bullets <- function() {
  c("Run `data-raw/stan-scripts.R` to update precompiled Stan code")
}
