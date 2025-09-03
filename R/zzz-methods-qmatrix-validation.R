#' Q-matrix validation
#'
#' Calculate Q-matrix validation metrics for a fitted model objects using
#' methods described by de la Torre and Chiu (2016). See details for additional
#' information.
#'
#' @param x A [measrdcm][dcm_estimate()] object.
#' @param pvaf_threshold The threshold for proportion of variance accounted for
#'   to flag items for appropriate empirical specifications. The default is .95
#'   as implemented by de la Torre and Chiu (2016).
#' @param ... Unused.
#'
#' @details
#' Q-matrix validation is conducted by evaluating the proporation of variance
#' accounted for by different Q-matrix specifications. Following the method
#' described by de la Torre and Chiu (2016), we use the following steps for
#' each item:
#' 1. Calculate the total variance explained if an item measured all possible
#'   attributes.
#' 2. For each possible Q-matrix entry, calculate the variance explained if the
#'   item measured the given attributes. Calculate the proportion of variance
#'   explained (PVAF) as the variance explained by the current Q-matrix entry
#'   divided by the variance explained by the saturated entry (Step 1).
#' 3. After computing the PVAF for all possible Q-matrix entries, filter to only
#'   those with a PVAF greater than the specified `pvaf_threshold` threshold.
#' 4. Filter the remaining Q-matrix entries to those that measure the fewest
#'   number of attributes (i.e., we prefer a more parsimonious model).
#' 5. If there is more than one Q-matrix entry remaining, select the entry with
#'   the highest PVAF.
#'
#' @concept Torre
#' @concept Chiu
#'
#' @return A [tibble][tibble::tibble-package] containing the Q-matrix
#' validation results. There is one row per item with 5 columns:
#' * The item identifier, as specified in the Q-matrix  and used to estimate the
#'   model.
#' * `original_specification`: The original Q-matrix entry for the item.
#' * `original_pvaf`: The proportion of variance accounted for by the original
#'   specification, compared to a specification where the item measures all
#'   attributes.
#' * `empirical_specification`: The Q-matrix specification that measures the
#'   fewest attributes with a proportion of variance accounted for over the
#'   the specified `pvaf_threshold` threshold. If the original specification is
#'   optimal, `empirical_specification` will be `NA`.
#' * `empirical_pvaf`: The proportion of variance accounted for by the empirical
#'   specification, compared to a specification where the item measures all
#'   attributes. If the original specification is optimal, `emprirical_pvaf`
#'   will be `NA`.
#'
#' @references de la Torre, J., & Chiu, C.-Y. (2016). A general method of
#'   empirical Q-matrix validation. *Psychometrika, 81*(2), 253-273.
#'   \doi{10.1007/s11336-015-9467-8}
#'
#' @name qmatrix_validation
#' @export
#' @examplesIf measr_examples()
#' mod_spec <- dcm_specify(
#'   qmatrix = dcmdata::ecpe_qmatrix,
#'   identifier = "item_id",
#'   measurement_model = dcmstan::lcdm(),
#'   structural_model = dcmstan::hdcm(
#'     hierarchy = "lexical -> cohesive -> morphosyntactic"
#'   )
#' )
#' rstn_ecpe <- dcm_estimate(
#'   mod_spec,
#'   data = dcmdata::ecpe_data,
#'   identifier = "resp_id",
#'   backend = "rstan",
#'   method = "optim"
#' )
#'
#' q_matrix_validation <- qmatrix_validation(rstn_ecpe)
qmatrix_validation <- S7::new_generic(
  "qmatrix_validation",
  "x",
  function(x, ..., pvaf_threshold = .95) {
    S7::S7_dispatch()
  }
)

# methods ----------------------------------------------------------------------
S7::method(qmatrix_validation, measrdcm) <- function(
  x,
  pvaf_threshold = .95
) {
  if (ncol(x@model_spec@qmatrix) == 1) {
    rlang::abort(
      "error_bad_method",
      message = glue::glue(
        "The Q-matrix validation method can ",
        "only be applied to assessments ",
        "measuring more than one attribute."
      )
    )
  }

  if (rlang::is_empty(x@respondent_estimates)) {
    x <- add_respondent_estimates(x)
  }

  qmatrix <- x@model_spec@qmatrix
  all_profiles <- create_profiles(x@model_spec)
  names(qmatrix) <- names(all_profiles)

  pi_mat <- get_draws(x, vars = c("pi")) |>
    posterior::subset_draws(variable = "pi") |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "parameter",
      values_to = "pi"
    ) |>
    dplyr::filter(!(.data$parameter %in% c(".chain", ".iteration", ".draw"))) |>
    dplyr::mutate(
      parameter = sub("pi\\[", "", .data$parameter),
      parameter = sub("]", "", .data$parameter)
    ) |>
    tidyr::separate_wider_delim(
      cols = "parameter",
      delim = ",",
      names = c("item_id", "profile_id")
    ) |>
    dplyr::select("profile_id", "item_id", "prob" = "pi") |>
    dplyr::mutate(
      profile_id = as.numeric(.data$profile_id),
      item_id = as.numeric(.data$item_id)
    )

  if (S7::S7_inherits(x@method, mcmc)) {
    pi_mat <- pi_mat |>
      dplyr::group_by(.data$profile_id, .data$item_id) |>
      dplyr::summarize(prob = mean(.data$prob), .groups = "keep") |>
      dplyr::ungroup()
  }

  # posterior probabilities of each class
  strc_param <- measr_extract(x, "strc_param")
  strc_param <- strc_param |>
    dplyr::mutate(estimate = E(.data$estimate)) |>
    dplyr::select("class", "estimate") |>
    dplyr::mutate(class = sub("\\[", "", class), class = sub("]", "", class))

  validation_output <- tibble::tibble()

  # create set of all possible Q-matrix specifications
  all_qmatrix_specifications <- create_profiles(ncol(qmatrix))
  colnames(all_qmatrix_specifications) <- colnames(all_profiles)

  # calculate sigma_1:K* (e.g., sigma_1:2)
  for (ii in seq_len(nrow(qmatrix))) {
    max_specification <- all_profiles[nrow(all_profiles), ]
    max_sigma <- calc_sigma(
      q = max_specification,
      strc_param = strc_param,
      pi_mat = pi_mat,
      ii
    )

    max_specification <- max_specification |>
      dplyr::mutate(pvaf = 1)

    possible_specifications <- tibble::tibble()
    possible_specifications <- dplyr::bind_rows(
      possible_specifications,
      max_specification
    )

    # iterate through the 2^K - 2 possible specifications for each item to
    # calculate sigma (e.g., sigma_1:3)
    for (jj in 2:(nrow(all_qmatrix_specifications) - 1)) {
      q <- all_qmatrix_specifications[jj, ]
      sigma_q <- calc_sigma(
        q = q,
        strc_param = strc_param,
        pi_mat = pi_mat,
        ii = ii
      )

      # calculate sigma / sigma_1:K (i.e., PVAF)
      pvaf <- sigma_q / max_sigma
      q <- q |>
        dplyr::mutate(pvaf = pvaf)

      # flagging profiles where sigma / sigma_1:K >= pvaf_threshold
      # only profiles where sigma / sigma_1:K >= pvaf_threshold are appropriate
      keep_spec <- (sigma_q / max_sigma) >= pvaf_threshold
      if (keep_spec) {
        possible_specifications <- dplyr::bind_rows(possible_specifications, q)
      }
    }

    # choosing profile measuring the fewest attributes
    # when there is a tie, profile chosen based on proportion of variance
    # accounted for (PVAF)
    correct_spec <- possible_specifications |>
      dplyr::select(-"pvaf") |>
      dplyr::mutate(
        total_atts = rowSums(dplyr::across(dplyr::where(is.numeric)))
      ) |>
      dplyr::filter(.data$total_atts == min(.data$total_atts)) |>
      dplyr::select(-"total_atts") |>
      dplyr::left_join(possible_specifications, by = colnames(all_profiles)) |>
      dplyr::filter(.data$pvaf == max(.data$pvaf))

    final_pvaf <- correct_spec |>
      dplyr::pull(.data$pvaf)

    correct_spec <- correct_spec |>
      dplyr::select(-"pvaf")

    actual_spec <- qmatrix[ii, ]
    original_sigma <- calc_sigma(
      q = actual_spec,
      strc_param = strc_param,
      pi_mat = pi_mat,
      ii = ii
    )
    validation_flag <- nrow(dplyr::anti_join(
      correct_spec,
      actual_spec,
      by = colnames(actual_spec)
    )) !=
      0

    item_output <- tibble::tibble(
      item_id = ii,
      validation_flag = validation_flag,
      original_specification = list(actual_spec),
      original_pvaf = original_sigma / max_sigma,
      empirical_specification = list(correct_spec),
      empirical_pvaf = final_pvaf
    )
    validation_output <- dplyr::bind_rows(validation_output, item_output)
  }

  validation_output |>
    dplyr::mutate(
      original_specification = vapply(
        X = .data$original_specification,
        FUN = \(dat) paste0("[", paste(dat[1, ], collapse = ", "), "]"),
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      ),
      empirical_specification = vapply(
        X = .data$empirical_specification,
        FUN = \(dat) paste0("[", paste(dat[1, ], collapse = ", "), "]"),
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      ),
      empirical_specification = mapply(
        FUN = \(valid, spec) if (!valid) NA_character_ else spec,
        valid = .data$validation_flag,
        spec = .data$empirical_specification,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
      ),
      empirical_pvaf = mapply(
        FUN = \(valid, pvaf) if (!valid) NA_character_ else pvaf,
        valid = .data$validation_flag,
        pvaf = .data$empirical_pvaf,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
      ),
      item_id = names(x@model_spec@qmatrix_meta$item_names)[.data$item_id]
    ) |>
    dplyr::rename(
      !!x@model_spec@qmatrix_meta$item_identifier := "item_id"
    ) |>
    dplyr::select(-"validation_flag")
}
