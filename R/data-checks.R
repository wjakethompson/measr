check_file <- function(
  x,
  create_dir = FALSE,
  check_file = TRUE,
  ext = NULL,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(character())
  }

  directory <- fs::path_dir(x)
  if (!fs::dir_exists(directory) && !create_dir) {
    rdcmchecks::abort_bad_argument(
      arg = arg,
      must = "be an existing directory",
      call = call
    )
  } else if (!fs::dir_exists(directory) && create_dir) {
    fs::dir_create(directory)
  }

  if (!is.null(ext)) {
    x <- fs::path_ext_set(x, ext = ext)
  }

  if (check_file && !fs::file_exists(x)) {
    rdcmchecks::abort_bad_argument(
      arg = arg,
      must = "be an existing file",
      call = call
    )
  }

  return(x)
}
