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

profile_labels <- function(attributes) {
  create_profiles(attributes) %>%
    tibble::rowid_to_column(var = "class_id") %>%
    tidyr::pivot_longer(cols = -"class_id") %>%
    dplyr::summarize(
      class = paste0("[", paste(.data$value, collapse = ","), "]"),
      .by = "class_id"
    ) %>%
    dplyr::arrange("class_id")
}

#' Get a list of possible parameters
#'
#' When specifying prior distributions, it is often useful to see which
#' parameters are included in a given model. Using the Q-matrix and type of
#' diagnostic model to estimated, we can create a list of all included
#' parameters for which a prior can be specified.
#'
#' @inheritParams measr_dcm
#' @param item_id Optional. Variable name of a column in `qmatrix` that contains
#'   item identifiers. `NULL` (the default) indicates that no identifiers are
#'   present in the Q-matrix.
#' @param rename_att Should attribute names from the `qmatrix` be replaced with
#'   generic, but consistent names (e.g., "att1", "att2", "att3").
#' @param rename_item Should item names from the `qmatrix` be replaced with
#'   generic, but consistent names (e.g., 1, 2, 3).
#'
#' @return A [tibble][tibble::tibble-package] with one row per parameter.
#' @export
#'
#' @examples
#' get_parameters(ecpe_qmatrix, item_id = "item_id", type = "lcdm")
#'
#' get_parameters(ecpe_qmatrix, item_id = "item_id", type = "lcdm",
#'                rename_att = TRUE)
get_parameters <- function(qmatrix, item_id = NULL, rename_att = FALSE,
                           rename_item = FALSE,
                           type = c("lcdm", "dina", "dino", "crum"),
                           attribute_structure = c("unconstrained",
                                                   "independent")) {
  item_id <- check_character(item_id, name = "item_id", allow_null = TRUE)
  qmatrix <- check_qmatrix(qmatrix, identifier = item_id, item_levels = NULL,
                           name = "qmatrix")
  att_names <- colnames(qmatrix)[which(!(colnames(qmatrix) == "item_id"))]

  item_ids <- qmatrix %>%
    dplyr::select("item_id") %>%
    tibble::rowid_to_column(var = "item_number")

  qmatrix <- qmatrix %>%
    dplyr::select(-"item_id") %>%
    dplyr::rename_with(~glue::glue("att{1:(ncol(qmatrix) - 1)}"),
                       .cols = dplyr::everything())

  type <- rlang::arg_match(type, dcm_choices())
  max_interaction <- ifelse(type == "crum", 1L, Inf)
  attribute_structure <- rlang::arg_match(attribute_structure, strc_choices())

  all_params <- if (type %in% c("dina", "dino")) {
    tidyr::expand_grid(item_id = seq_len(nrow(qmatrix)),
                       class = c("slip", "guess")) %>%
      dplyr::mutate(
        coef = glue::glue("{.data$class}[{.data$item_id}]")
      )
  } else if (type %in% c("lcdm", "crum")) {
    stats::model.matrix(stats::as.formula(paste0("~ .^",
                                                 max(ncol(qmatrix), 2L))),
                        qmatrix) %>%
      tibble::as_tibble(.name_repair = model_matrix_name_repair) %>%
      dplyr::select(dplyr::where(~ sum(.x) > 0)) %>%
      tibble::rowid_to_column(var = "item_id") %>%
      tidyr::pivot_longer(cols = -"item_id", names_to = "parameter",
                          values_to = "value") %>%
      dplyr::filter(.data$value == 1) %>%
      dplyr::mutate(
        param_level = dplyr::case_when(
          .data$parameter == "intercept" ~ 0,
          !grepl("__", .data$parameter) ~ 1,
          TRUE ~ sapply(gregexpr(pattern = "__", text = .data$parameter),
                        function(.x) length(attr(.x, "match.length"))) + 1
        ),
        atts = gsub("[^0-9|_]", "", .data$parameter),
        coef = glue::glue("l{item_id}_{param_level}",
                          "{gsub(\"__\", \"\", atts)}"),
        class = dplyr::case_when(.data$param_level == 0 ~ "intercept",
                                 .data$param_level == 1 ~ "maineffect",
                                 .data$param_level >= 2 ~ "interaction"),
        attributes = dplyr::case_when(.data$param_level == 0 ~ NA_character_,
                                      .data$param_level >= 1 ~ .data$parameter)
      ) %>%
      dplyr::filter(.data$param_level <= max_interaction) %>%
      dplyr::select("item_id", "class", "attributes", "coef")
  }

  if (!rename_att && ("attributes" %in% colnames(all_params))) {
    for (i in seq_along(att_names)) {
      all_params <- dplyr::mutate(all_params,
                                  attributes = gsub(paste0("att", i),
                                                    att_names[i],
                                                    .data$attributes))
    }
  }

  if (!rename_item) {
    all_params <- all_params %>%
      dplyr::left_join(item_ids, by = c("item_id" = "item_number")) %>%
      dplyr::mutate(item_id = .data$item_id.y) %>%
      dplyr::select(-"item_id.y")
  }

  strc_params <- if (attribute_structure == "unconstrained") {
    tibble::tibble(class = "structural", coef = "Vc")
  } else if (attribute_structure == "independent") {
    tibble::tibble(class = "structural",
                   coef = glue::glue("eta[{seq_len(length(att_names))}]"))
  }


  all_params <- dplyr::bind_rows(all_params, strc_params)

  return(all_params)
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
  try <- check_logical(try, name = "try")
  silent <- check_logical(silent, name = "silent")
  type <- match.arg(type, c("output", "message"))
  expr <- substitute(expr)
  envir <- parent.frame()
  if (silent) {
    if (try && type == "message") {
      try_out <- try(utils::capture.output(
        out <- eval(expr, envir), type = type, ...
      ))
      if (methods::is(try_out, "try-error")) {
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
