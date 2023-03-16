#' @importFrom dcm2 fit_m2
#' @export
dcm2::fit_m2

#' Estimate the \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} fit statistic for
#' diagnostic classification models
#'
#' For diagnostic classification models, the
#' \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} statistic is calculated as
#' described by Hansen et al. (2016) and Liu et al. (2016).
#'
#' @references Hansen, M., Cai, L., Monroe, S., & Li, Z. (2016).
#'   Limited-information goodness-of-fit testing of diagnostic classification
#'   item response models. *British Journal of Mathematical and Statistical
#'   Psychology, 69*(3), 225-252. \url{https://doi.org/10.1111/bmsp.12074}
#' @references Liu, Y., Tian, W., & Xin, T. (2016). An application of
#'   \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} statistic to evaluate the fit
#'   of cognitive diagnostic models. *Journal of Educational and Behavioral
#'   Statistics, 41*(1), 3-26. \url{https://doi.org/10.3102/1076998615621293}
#'
#' @describeIn fit_m2 \ifelse{html}{\out{M<sub>2</sub>}}{\eqn{M_2}} for
#'   diagnostic classification models.
#' @export
fit_m2.measrdcm <- function(model, ci = 0.9) {

}


