abort_bad_argument <- function(arg, must, not = NULL, extra = NULL,
                               custom = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not {not}")
  }
  if (!is.null(extra)) {
    msg <- glue::glue("{msg}", "{extra}", .sep = "\n")
  }
  if (!is.null(custom)) {
    msg <- custom
  }

  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not)
}

check_model <- function(x, required_class, name, list = FALSE) {
  if (!(all(required_class %in% class(x)) && (typeof(x) == "list"))) {
    must <- if (list) {
      glue::glue("only contain objects with class ",
                 "{paste(required_class, collapse = ',')}")
    } else {
      glue::glue("be an object with class ",
                 "{paste(required_class, collapse = ',')}")
    }

    abort_bad_argument(name, must = must)
  }

  x
}

check_newdata <- function(x, name, identifier, model, missing) {
  x <- check_data(x, name = name, identifier = identifier,
                  missing = missing)

  if (!all(levels(x$item_id) %in% levels(model$data$data$item_id))) {
    good_items <- levels(model$data$data$item_id)
    bad_items <- levels(x$item_id)[!(levels(x$item_id) %in% good_items)]
    msg <- paste0("New items found in `newdata`: ",
                  paste(bad_items, collapse = " "))
    abort_bad_argument(name, must = NULL, custom = msg)
  }

  # ensure that factor levels match in original and new data so item parameters
  # are pulled correctly
  x <- x %>%
    dplyr::mutate(
      item_id = as.character(.data$item_id),
      item_id = factor(.data$item_id, levels = levels(model$data$data$item_id))
    ) %>%
    dplyr::arrange(.data$resp_id, .data$item_id)

  x
}

check_data <- function(x, name, identifier, missing) {
  if (!is.null(identifier)) identifier <- enquo(identifier)

  if (!("data.frame" %in% class(x))) {
    abort_bad_argument(name, must = "be a data frame")
  }

  # replace missing values with NA
  x <- x %>%
    dplyr::mutate(dplyr::across(!dplyr::all_of(!!identifier),
                                ~dplyr::na_if(.x, missing)),
                  dplyr::across(!dplyr::all_of(!!identifier), as.character))

  if (!all(sapply(dplyr::select(x, -!!identifier),
                  function(.x) all(.x %in% c("0", "1", NA_character_))))) {
    abort_bad_argument(name,
                       must = "contain only 0 or 1 for non-missing scores")
  }

  item_names <- colnames(x)
  if (!is.null(identifier)) {
    item_names <- item_names[-which(item_names == rlang::as_name(identifier))]
  }

  # move to long format for Stan
  if (is.null(identifier)) {
    x <- x %>%
      tibble::rowid_to_column(var = "resp_id") %>%
      tidyr::pivot_longer(cols = -"resp_id", names_to = "item_id",
                          values_to = "score") %>%
      dplyr::filter(!is.na(.data$score)) %>%
      dplyr::mutate(score = as.integer(.data$score),
                    resp_id = factor(.data$resp_id),
                    item_id = factor(.data$item_id, levels = item_names))
  } else {
    resp_names <- dplyr::pull(x, !!identifier)
    x <- x %>%
      tidyr::pivot_longer(cols = -!!identifier, names_to = "item_id",
                          values_to = "score") %>%
      dplyr::filter(!is.na(.data$score)) %>%
      dplyr::rename(resp_id = !!identifier) %>%
      dplyr::mutate(score = as.integer(.data$score),
                    resp_id = factor(.data$resp_id, levels = resp_names),
                    item_id = factor(.data$item_id, levels = item_names))
  }

  x <- x %>%
    dplyr::arrange(.data$resp_id, .data$item_id)

  x
}

check_qmatrix <- function(x, identifier, item_levels, name) {
  if (!is.null(identifier)) identifier <- enquo(identifier)

  if (!("data.frame" %in% class(x))) {
    abort_bad_argument(name, must = "be a data frame")
  }

  if (nrow(x) != length(item_levels) && !is.null(item_levels)) {
    abort_bad_argument(name, must = glue::glue("have the same number of rows ",
                                               "as columns of items in `data`"))
  }

  #check that item ids match item levels
  x <- check_item_levels(x, identifier, item_levels, name)

  if (!all(sapply(dplyr::select(x, -"item_id"), is.numeric))) {
    abort_bad_argument(name, must = "contain only numeric columns")
  }
  x <- dplyr::mutate(x, dplyr::across(-"item_id", as.integer))

  if (!all(sapply(dplyr::select(x, -"item_id"),
                  function(.x) all(.x %in% c(0L, 1L))))) {
    abort_bad_argument(name, must = "contain only 0 or 1 in attribute columns")
  }

  if (!tibble::is_tibble(x)) {
    tibble::as_tibble(x)
  } else {
    x
  }
}

check_item_levels <- function(x, identifier, item_levels, name) {
  if (is.null(identifier) && !is.null(item_levels)) {
    x <- x %>%
      dplyr::mutate(item_id = item_levels,
                    item_id = factor(.data$item_id, levels = item_levels),
                    .before = 1)
  } else if (!is.null(item_levels)) {
    item_names <- dplyr::pull(x, !!identifier)
    if (!all(item_levels %in% item_names)) {
      abort_bad_argument(
        name,
        must = glue::glue("include all items in `data`.
                          Missing items: {setdiff(item_levels, item_names)}")
      )
    }
    if (!all(item_names %in% item_levels)) {
      abort_bad_argument(
        name,
        must = glue::glue("only include items found in `data`.
                          Extra items: {paste(setdiff(item_names, item_levels),
                                                  collapse = ', ')}")
      )
    }
    x <- x %>%
      dplyr::rename(item_id = !!identifier) %>%
      dplyr::mutate(item_id = factor(.data$item_id, levels = item_levels)) %>%
      dplyr::arrange(.data$item_id)
  } else if (is.null(identifier) && is.null(item_levels)) {
    x <- x %>%
      dplyr::mutate(item_id = seq_len(dplyr::n()),
                    item_id = factor(.data$item_id, levels = .data$item_id),
                    .before = 1) %>%
      dplyr::arrange(.data$item_id)
  } else if (!is.null(identifier) && is.null(item_levels)) {
    x <- x %>%
      dplyr::rename(item_id = !!identifier) %>%
      dplyr::mutate(item_id = factor(.data$item_id, levels = .data$item_id)) %>%
      dplyr::arrange(.data$item_id)
  }

  return(x)
}

check_prior <- function(x, name, allow_null = FALSE) {
  if (allow_null && is.null(x)) return(x)

  if (!is.measrprior(x)) {
    abort_bad_argument(name, must = "be a measrprior object")
  }

  x
}

check_file <- function(x, name, create_dir = FALSE, check_file = TRUE,
                       ext = NULL, allow_null = FALSE) {
  if (allow_null && is.null(x)) return(x)

  directory <- fs::path_dir(x)
  if (!fs::dir_exists(directory) && !create_dir) {
    abort_bad_argument(name, must = "be an existing directory")
  } else if (!fs::dir_exists(directory) && create_dir) {
    fs::dir_create(directory)
  }

  if (!is.null(ext)) {
    x <- fs::path_ext_set(x, ext = ext)
  }

  if (check_file && !fs::file_exists(x)) {
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

  if (is.na(x) && !allow_na) {
    abort_bad_argument(name, must = "be non-missing")
  }

  x
}

check_integer <- function(x, lb = -Inf, ub = Inf, inclusive = TRUE,
                          allow_null = FALSE, name) {
  if (is.null(x) && allow_null) return(x)

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

check_double <- function(x, lb = -Inf, ub = Inf, inclusive = TRUE, name) {
  if (inclusive) {
    check_lb <- function(.x, .y) x < .y
    check_ub <- function(.x, .y) x > .y
  } else if (!inclusive) {
    check_lb <- function(.x, .y) x <= .y
    check_ub <- function(.x, .y) x >= .y
  }

  if (!is.numeric(x)) {
    abort_bad_argument(name, must = "be a numeric scalar", not = typeof(x))
  }

  if (length(x) != 1) {
    abort_bad_argument(name, must = "be of length 1", not = length(x))
  }

  if (is.na(x)) {
    abort_bad_argument(name, must = "be non-missing")
  }

  if (check_lb(x, lb) || check_ub(x, ub)) {
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
check_double <- Vectorize(check_double)

check_character <- function(x, allow_null = FALSE, allow_na = FALSE, name) {
  if (is.null(x) && allow_null) {
    return(x)
  }

  if (any(is.na(x))) {
    x[is.na(x)] <- NA_character_
  }

  if (!is.character(x)) {
    abort_bad_argument(name, must = "be a character scalar", not = typeof(x))
  }

  if (length(x) != 1) {
    abort_bad_argument(name, must = "be of length 1", not = length(x))
  }

  if (is.na(x) && !allow_na) {
    abort_bad_argument(name, must = "be non-missing")
  }

  x
}
