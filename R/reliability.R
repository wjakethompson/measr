#' @export
reliability <- function(model) {
  UseMethod("reliability")
}

#' Estimate the reliability of a diagnostic classification model
#'
#' Estimate the reliability of an estimated diagnostic classification model at
#' the pattern or attribute level. Pattern-level reliability represents the
#' classification consistency and accuracy of placing students into an overall
#' mastery profile. Rather than an overall profile, attributes can also be
#' scored individually. In this case, classification consistency and accuracy
#' should be evaluated for each individual attribute, rather than the overall
#' profile. This is referred to as the *modal a posteriori* (MAP) reliability.
#' Finally, it may be desirable to report results as the probability of
#' proficiency or mastery on each attribute instead of a proficient/not
#' proficient classification. In this case, the reliability of the posterior
#' probability should be reported. THis is the *expected a poteriori* (EAP)
#' reliability.
#'
#' @param model A DCM estimated by [measr_dcm()].
#'
#' @details The pattern-level reliability (`pattern_reliability`) statistics are
#' described in Cui et al. (2012). Attribute-level classification reliability
#' statistics (`map_reliability`) are described in Johnson & Sinharay (2018).
#' Reliability statistics for the posterior mean of the skill indicators (i.e.,
#' the mastery or proficiency probabilities; `eap_reliability`) are described in
#' Johnson & Sinharay (2019).
#'
#' @returns A list with 3 elements:
#'   * `pattern_reliability`: The pattern-level accuracy (`p_a`) and consistency
#'     (`p_c`) described by Cui et al. (2012).
#'   * `map_reliability`: A list with 2 elements: `accuracy` and `consistency`,
#'     which include the attribute-level classification reliability statistics
#'     described by Johnson & Sinharay (2018).
#'   * `eap_reliability`: The attribute-level posterior probability reliability
#'     statistics described by Johnson & Sinharay (2020).
#'
#' @references Cui, Y., Gierl, M. J., & Chang, H.-H. (2012). Estimating
#'   classification consistency and accuracy for cognitive diagnostic
#'   assessment. *Journal of Educational Measurement, 49*(1), 19-38.
#'   \url{https://doi.org/10.1111/j.1745-3984.2011.00158.x}
#' @references Johnson, M. S., & Sinharay, S. (2018). Measures of agreement to
#'   assess attribute-level classification accuracy and consistency for
#'   cognitive diagnostic assessments. *Journal of Educational Measurement,
#'   55*(4), 635-664. \url{https://doi.org/10.1111/jedm.12196}
#' @references Johnson, M. S., & Sinharay, S. (2020). The reliability of the
#'   posterior probability of skill attainment in diagnostic classification
#'   models. *Journal of Educational and Behavioral Statistics, 45*(1), 5-31.
#'   \url{https://doi.org/10.3102/1076998619864550}
#'
#' @export
reliability.measrdcm <- function(model) {
  # coerce model into a list of values required for reliability
  obj <- reli_list(model)
  att_names <- model$data$qmatrix %>%
    dplyr::select(-"item_id") %>%
    colnames()

  tbl <- obj$acc
  p <- obj$prev
  k <- length(p)
  acc <- apply(tbl, 3, function(m) {
    sum(diag(m)) / sum(m)
  })
  tp_a <- apply(tbl, 3, function(m) {
    m[2, 2] / (sum(m[, 2]))
  })
  tn_a <- apply(tbl, 3, function(m) {
    m[1, 1] / sum(m[, 1])
  })
  youden_a <- tp_a + tn_a - 1
  mx <- apply(tbl, 3, function(m) {
    max(apply(m / sum(m), 2, sum))
  })
  lambda_a <- (acc - mx) / (1 - mx)
  tetra_a <- apply(tbl, 3, function(m) {
    psych::tetrachoric(m)$rho
  })
  kap_base <- p * p + (1 - p) * (1 - p)
  kappa_a <- (acc - kap_base) / kap_base

  tbl <- obj$consist

  consist <- apply(tbl, 3, function(m) {
    m[m < 1e-10] <- 0
    sum(diag(m)) / sum(m)
  })
  tp_c <- apply(tbl, 3, function(m) {
    m[m < 1e-10] <- 0
    m[2, 2] / (sum(m[, 2]))
  })
  tn_c <- apply(tbl, 3, function(m) {
    m[m < 1e-10] <- 0
    m[1, 1] / sum(m[, 1])
  })
  youden_c <- tp_c + tn_c - 1
  mx <- apply(tbl, 3, function(m) {
    m[m < 1e-10] <- 0
    max(apply(m / sum(m), 2, sum))
  })
  lambda_c <- (consist - mx) / (1 - mx)

  tetra_c <- apply(tbl, 3, function(m) {
    m[m < 1e-10] <- 0
    psych::tetrachoric(m)$rho
  })
  p <- apply(tbl, 3, function(m) {
    sum(m[2, ])
  })
  kap_base <- p * p + (1 - p) * (1 - p)
  kappa_c <- (acc - kap_base) / kap_base

  all_a <- as.matrix(create_profiles(k))
  p_prime <- apply(sweep(apply(obj$posterior, 2, function(v) {
    out <- tapply(v, apply(obj$posterior, 1, which.max), sum) / length(v)
    if (dim(out) == 1) out <- as.array(c(out, `2` = 0))
    return(out)
  }),
  2, obj$strc, "/") ^ 2, 2, sum)
  pc <- sum(p_prime * obj$strc)
  pc1 <- p_prime %*% (obj$strc * all_a) / p
  pc0 <- p_prime %*% (obj$strc * (1 - all_a)) / (1 - p)
  pc_prime <- as.vector(pc1 * pc1 * p + pc0 * pc0 * (1 - p))
  pa <- mean(apply(obj$posterior, 1, max))

  res_map_acc <- list(tibble::enframe(acc, name = "attribute", value = "acc"),
                      tibble::enframe(lambda_a, name = "attribute",
                                      value = "lambda_a"),
                      tibble::enframe(kappa_a, name = "attribute",
                                      value = "kappa_a"),
                      tibble::enframe(youden_a, name = "attribute",
                                      value = "youden_a"),
                      tibble::enframe(tetra_a, name = "attribute",
                                      value = "tetra_a"),
                      tibble::enframe(tp_a, name = "attribute", value = "tp_a"),
                      tibble::enframe(tn_a, name = "attribute",
                                      value = "tn_a")) %>%
    purrr::reduce(dplyr::full_join, by = "attribute") %>%
    tibble::remove_rownames()

  gammak <- purrr::set_names(obj$gamma, att_names)
  pc_prime <- purrr::set_names(pc_prime, att_names)
  res_map_con <- list(tibble::enframe(consist, name = "attribute",
                                      value = "consist"),
                      tibble::enframe(lambda_c, name = "attribute",
                                      value = "lambda_c"),
                      tibble::enframe(kappa_c, name = "attribute",
                                      value = "kappa_c"),
                      tibble::enframe(youden_c, name = "attribute",
                                      value = "youden_c"),
                      tibble::enframe(tetra_c, name = "attribute",
                                      value = "tetra_c"),
                      tibble::enframe(tp_c, name = "attribute", value = "tp_c"),
                      tibble::enframe(tn_c, name = "attribute", value = "tn_c"),
                      tibble::enframe(gammak, name = "attribute",
                                      value = "gammak"),
                      tibble::enframe(pc_prime, name = "attribute",
                                      value = "pc_prime")) %>%
    purrr::reduce(dplyr::full_join, by = "attribute") %>%
    tibble::remove_rownames()

  ## Reliability for EAP  ##
  tmp_fun <- function(v) {
    m <- matrix(c(sum(v * v), sum(v * (1 - v)), sum(v * (1 - v)),
                  sum((1 - v) * (1 - v))), 2, 2)
    rho_tb <- psych::tetrachoric(m)$rho
    rho_bs <- m[2, 2] / (m[2, 2] + m[2, 1]) - m[1, 2] / (m[1, 1] + m[1, 2])
    ind_info <- ifelse(.5 - abs(v - .5) < 1e-12, 0, v * log(v) + (1 - v) *
                         log(1 - v))
    post_info <- mean(ind_info)
    prior <- mean(v)
    prior_info <- prior * log(prior) + (1 - prior) * log(1 - prior)
    rho_i <- 1 - exp(-2 * (post_info - prior_info))
    pf_num <- sum(apply(v * obj$posterior, 2, mean) ^ 2 / obj$strc) - prior ^ 2
    pf_den <- mean(v * v) - prior ^ 2
    rho_pf <- pf_num / pf_den
    c(rho_pf, rho_bs, rho_i, rho_tb)
  }
  res_eap <- apply(obj$eap, 2, tmp_fun)
  res_eap <- t(res_eap)

  res_eap <- as.data.frame(res_eap) %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(c("rho_pf", "rho_bs", "rho_i", "rho_tb")) %>%
    tibble::add_column(attribute = att_names, .before = 1)

  list(pattern_reliability = c(p_a = pa, p_c = pc),
       map_reliability = list(accuracy = res_map_acc,
                              consistency = res_map_con),
       eap_reliability = res_eap)
}
