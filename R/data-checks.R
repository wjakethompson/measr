abort_bad_argument <- function(arg, must, not = NULL, extra = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not {not}")
  }
  if (!is.null(extra)) {
    msg <- glue::glue("{msg}", "{extra}", .sep = "\n")
  }

  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not)
}

check_data <- function(x, name, identifier, missing) {
  if (!is.null(identifier)) identifier <- enquo(identifier)

  if (!("data.frame" %in% class(x))) {
    abort_bad_argument(name, must = "be a data frame")
  }

  if (!all(sapply(dplyr::select(x, -!!identifier), is.numeric))) {
    abort_bad_argument(name, must = "contain only numeric columns")
  }
  x <- dplyr::mutate(x, dplyr::across(!dplyr::all_of(!!identifier), as.integer))


  item_names <- colnames(x)
  if (!is.null(identifier)) {
    item_names <- item_names[-which(item_names == rlang::as_name(identifier))]
  }

  # move to long format for Stan
  if (is.null(identifier)) {
    x <- x %>%
      tibble::rowid_to_column(var = "resp_id") %>%
      tidyr::pivot_longer(cols = -.data$resp_id, names_to = "item_id",
                          values_to = "score") %>%
      dplyr::filter(!(.data$score %in% missing)) %>%
      dplyr::mutate(score = as.integer(.data$score),
                    resp_id = factor(.data$resp_id),
                    item_id = factor(.data$item_id, levels = item_names))
  } else {
    resp_names <- dplyr::pull(x, !!identifier)
    x <- x %>%
      tidyr::pivot_longer(cols = -!!identifier, names_to = "item_id",
                          values_to = "score") %>%
      dplyr::filter(!(.data$score %in% missing)) %>%
      dplyr::rename(resp_id = !!identifier) %>%
      dplyr::mutate(score = as.integer(.data$score),
                    resp_id = factor(.data$resp_id, levels = resp_names),
                    item_id = factor(.data$item_id, levels = item_names))
  }

  if (!all(x$score %in% c(0L, 1L))) {
    abort_bad_argument(name,
                       must = "contain only 0 or 1 for non-missing scores")
  }

  # x <- x %>%
  #   dplyr::left_join(
  #     tibble::rowid_to_column(x %>%
  #                               dplyr::select(dplyr::all_of(!!identifier)) %>%
  #                               dplyr::distinct(),
  #                             var = "stan_resp_id"),
  #     by = rlang::as_name(identifier)) %>%
  #   dplyr::left_join(
  #     tibble::rowid_to_column(dplyr::distinct(x, .data$item_id),
  #                             var = "stan_item_id"),
  #     by = "item_id") %>%
  #   dplyr::select(dplyr::all_of(!!identifier), .data$stan_resp_id,
  #                 .data$item_id, .data$stan_item_id,
  #                 .data$score) %>%
  #   dplyr::arrange(.data$stan_resp_id, .data$stan_item_id)

  x <- x %>%
    dplyr::arrange(.data$resp_id, .data$item_id)

  if (!tibble::is_tibble(x)) {
    tibble::as_tibble(x)
  } else {
    x
  }
}

check_qmatrix <- function(x, identifier, item_levels, name) {
  if (!is.null(identifier)) identifier <- enquo(identifier)

  if (!("data.frame" %in% class(x))) {
    abort_bad_argument(name, must = "be a data frame")
  }

  if (nrow(x) != length(item_levels)) {
    abort_bad_argument(name, must = glue::glue("have the same number of rows ",
                                               "as columns of items in `data`"))
  }

  #check that item ids match item levels
  if (is.null(identifier)) {
    x <- x %>%
      dplyr::mutate(item_id = item_levels,
                    item_id = factor(.data$item_id, levels = item_levels),
                    .before = 1)
  } else {
    item_names <- dplyr::pull(x, !!identifier)
    if (!all(item_levels %in% item_names)) {
      missing_items <- setdiff(item_levels, item_names)
      abort_bad_argument(name,
                         must = glue::glue("include all items in `data`.
                                           Missing items: {missing_items}"))
    }
    if (!all(item_names %in% item_levels)) {
      extra_items <- setdiff(item_names, item_levels)
      abort_bad_argument(name,
                         must = glue::glue("only include items found in `data`.
                                           Extra items: {extra_items}"))
    }
    x <- x %>%
      dplyr::rename(item_id = !!identifier) %>%
      dplyr::mutate(item_id = factor(.data$item_id, levels = item_levels)) %>%
      dplyr::arrange(.data$item_id)
  }

  if (!all(sapply(dplyr::select(x, -.data$item_id), is.numeric))) {
    abort_bad_argument(name, must = "contain only numeric columns")
  }
  x <- dplyr::mutate(x, dplyr::across(!dplyr::all_of(!!identifier), as.integer))

  if (!all(sapply(dplyr::select(x, -.data$item_id),
                  function(.x) all(.x %in% c(0L, 1L))))) {
    abort_bad_argument(name, must = "contain only 0 or 1 in attribute columns")
  }

  if (!tibble::is_tibble(x)) {
    tibble::as_tibble(x)
  } else {
    x
  }
}

check_prior <- function(x, name, allow_null = FALSE) {
  if (allow_null & is.null(x)) return(x)

  if (!is.measrprior(x)) {
    abort_bad_argument(name, must = "be a measrprior object")
  }

  x
}

check_file <- function(x, name, create_dir = FALSE, check_file = TRUE,
                       ext = NULL, allow_null = FALSE) {
  if (allow_null & is.null(x)) return(x)

  directory <- fs::path_dir(x)
  if (!fs::dir_exists(directory) & !create_dir) {
    abort_bad_argument(name, must = "be an existing directory")
  } else if (!fs::dir_exists(directory) & create_dir) {
    fs::dir_create(directory)
  }

  if (!is.null(ext)) {
    x <- fs::path_ext_set(x, ext = ext)
  }

  if (check_file & !fs::file_exists(x)) {
    abort_bad_argument(name, must = "be an existing file")
  }

  return(x)
}

check_logical <- function(x, allow_na = FALSE, name) {
  if (!is.logical(x)) {
    abort_bad_argument(name, must = "be a logical scalar", not = typeof(x))
  }

  if (length(x) != 1) {
    abort_bad_argument(name, must = "be of length 1", not = length(x))
  }

  if (is.na(x) & !allow_na) {
    abort_bad_argument(name, must = "be non-missing")
  }

  x
}

check_integer <- function(x, lb = -Inf, ub = Inf, inclusive = TRUE, name) {
  if (inclusive) {
    check_lb <- lb
    check_ub <- ub
  } else if (!inclusive) {
    check_lb <- lb + 1L
    check_ub <- ub - 1L
  }

  if (!is.numeric(x)) {
    abort_bad_argument(name, must = "be a numeric scalar", not = typeof(x))
  }
  x <- as.integer(x)

  if (length(x) != 1) {
    abort_bad_argument(name, must = "be of length 1", not = length(x))
  }

  if (is.na(x)) {
    abort_bad_argument(name, must = "be non-missing")
  }

  if (x < check_lb || x > check_ub) {
    msg <- if (is.infinite(lb)) {
      glue::glue("be less than {ub}")
    } else if (is.infinite(ub)) {
      glue::glue("be greater than {lb}")
    } else {
      glue::glue("be between {lb} and {ub}")
    }
    abort_bad_argument(name, must = msg)
  }

  x
}

check_character <- function(x, allow_na = FALSE, name) {
  if (any(is.na(x))) {
    x[is.na(x)] <- NA_character_
  }

  if (!is.character(x)) {
    abort_bad_argument(name, must = "be a character scalar", not = typeof(x))
  }

  if (length(x) != 1) {
    abort_bad_argument(name, must = "be of length 1", not = length(x))
  }

  if (is.na(x) & !allow_na) {
    abort_bad_argument(name, must = "be non-missing")
  }

  x
}
