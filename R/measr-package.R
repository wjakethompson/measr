#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import dcmstan
#' @import methods
#' @import Rcpp
#' @import rlang
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom rstan sampling
#' @importFrom bridgesampling bridge_sampler
#' @importFrom tibble tibble
#' @useDynLib measr, .registration = TRUE
## usethis namespace: end
NULL

# nolint next
.datatable.aware <- TRUE

release_bullets <- function() {
  c("Run `data-raw/stan-scripts.R` to update precompiled Stan code")
}
