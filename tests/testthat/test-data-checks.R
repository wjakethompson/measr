test_that("multiplication works", {
  dir <- withr::local_tempdir()

  err <- rlang::catch_cnd(check_file(fs::path(dir, "directory", "file")))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "must be an existing directory")

  err <- rlang::catch_cnd(check_file(
    fs::path(dir, "directory", "file"),
    create_dir = TRUE
  ))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "must be an existing file")

  file <- fs::file_temp(tmp_dir = dir, ext = ".txt")
  err <- rlang::catch_cnd(check_file(file, create_dir = TRUE))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "must be an existing file")

  fs::file_create(file)
  err <- rlang::catch_cnd(check_file(file, ext = "rds", create_dir = TRUE))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "must be an existing file")

  expect_identical(
    check_file(file, create_dir = FALSE, check_file = TRUE),
    file
  )
  expect_identical(
    check_file(file, ext = "rds", check_file = FALSE),
    fs::path_ext_set(file, "rds")
  )
  expect_identical(
    check_file(fs::path_ext_set(file, "rds"), check_file = FALSE),
    fs::path_ext_set(file, "rds")
  )
  expect_identical(
    check_file(
      fs::path(dir, "directory", "file"),
      create_dir = TRUE,
      check_file = FALSE
    ),
    fs::path(dir, "directory", "file")
  )
  expect_identical(check_file(NULL, allow_null = TRUE), character())
})
