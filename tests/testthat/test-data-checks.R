test_that("check qmatrix", {
  err <- rlang::catch_cnd(check_qmatrix("a", identifier = NULL,
                                        item_levels = NA, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "data frame")

  test_q <- data.frame(item = paste0("I", 1:5),
                       att1 = sample(0:1, 5, replace = TRUE),
                       att2 = sample(0:1, 5, replace = TRUE))
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = NULL,
                                        item_levels = paste0("I", 1:5),
                                        name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "only numeric columns")

  test_q <- data.frame(item = sample(0:1, 5, replace = TRUE),
                       att2 = sample(1:2, 5, replace = TRUE),
                       att3 = sample(2:3, 5, replace = TRUE))
  expect_error(check_qmatrix(test_q, "check1", identifier = NULL,
                             item_levels = c(1:5)), regexp = "only 0 or 1")

  test_q <- data.frame(item = paste0("I", 1:5),
                       att1 = sample(0:1, 5, replace = TRUE),
                       att2 = sample(0:1, 5, replace = TRUE))
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = "item",
                                        item_levels = paste0("I", 1:6),
                                        name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "have the same number of rows as columns of items")

  test_q <- data.frame(item = paste0("I", 1:5),
                       att1 = sample(0:1, 5, replace = TRUE),
                       att2 = sample(0:1, 5, replace = TRUE))
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = "item",
                                        item_levels = paste0("I", c(1:4, 6)),
                                        name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "Missing items: I6")

  test_q <- data.frame(item = paste0("I", 1:5),
                       att1 = sample(0:1, 5, replace = TRUE),
                       att2 = sample(0:1, 5, replace = TRUE))
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = "item",
                                        item_levels = paste0("I", c(1:4, 4)),
                                        name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "Extra items: I5")

  test1_q <- tibble::tibble(item = paste0("I_", 1:5),
                            att1 = c(0, 1, 1, 0, 1),
                            att2 = c(1, 0, 0, 1, 0),
                            att3 = c(1, 1, 1, 0, 0))
  test2_q <- tibble::tibble(item = paste0("I_", 1:5),
                            att1 = c(0L, 1L, 1L, 0L, 1L),
                            att2 = c(1L, 0L, 0L, 1L, 0L),
                            att3 = c(1L, 1L, 1L, 0L, 0L))
  test3_q <- data.frame(item = paste0("I_", 1:5),
                        att1 = c(0, 1, 1, 0, 1),
                        att2 = c(1, 0, 0, 1, 0),
                        att3 = c(1, 1, 1, 0, 0))
  test4_q <- data.frame(item = paste0("I_", 1:5),
                        att1 = c(0L, 1L, 1L, 0L, 1L),
                        att2 = c(1L, 0L, 0L, 1L, 0L),
                        att3 = c(1L, 1L, 1L, 0L, 0L))
  check_q <- tibble::tibble(item_id = factor(paste0("I_", 1:5)),
                            att1 = c(0L, 1L, 1L, 0L, 1L),
                            att2 = c(1L, 0L, 0L, 1L, 0L),
                            att3 = c(1L, 1L, 1L, 0L, 0L))
  expect_identical(check_qmatrix(test1_q, identifier = "item",
                                 item_levels = paste0("I_", 1:5),
                                 name = "check1"), check_q)
  expect_identical(check_qmatrix(test2_q, identifier = "item",
                                 item_levels = paste0("I_", 1:5),
                                 name = "check1"), check_q)
  expect_identical(check_qmatrix(test3_q, identifier = "item",
                                 item_levels = paste0("I_", 1:5),
                                 name = "check1"), check_q)
  expect_identical(check_qmatrix(test4_q, identifier = "item",
                                 item_levels = paste0("I_", 1:5),
                                 name = "check1"), check_q)
  expect_identical(check_qmatrix(check_q, identifier = "item_id",
                                 item_levels = paste0("I_", 1:5),
                                 name = "check1"), check_q)
})

test_that("check_logical", {
  err <- rlang::catch_cnd(check_logical(1L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "logical scalar")
  expect_equal(err$not, "integer")

  err <- rlang::catch_cnd(check_logical(rep(TRUE, 3), name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "length 1")
  expect_equal(err$not, 3L)

  err <- rlang::catch_cnd(check_logical(NA, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "non-missing")

  expect_equal(check_logical(TRUE, name = "check1"), TRUE)
  expect_equal(check_logical(FALSE, name = "check1"), FALSE)
  expect_equal(check_logical(NA, allow_na = TRUE, name = "check1"), NA)
})

test_that("check_integer", {
  err <- rlang::catch_cnd(check_integer("a", name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "numeric scalar")
  expect_equal(err$not, "character")

  err <- rlang::catch_cnd(check_integer(1:2, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "length 1")
  expect_equal(err$not, 2L)

  err <- rlang::catch_cnd(check_integer(NA_integer_, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "non-missing")

  err <- rlang::catch_cnd(check_integer(-1, lb = 0L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "greater than 0")

  err <- rlang::catch_cnd(check_integer(1, ub = 0L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "less than 0")

  err <- rlang::catch_cnd(check_integer(4L, lb = 0L, ub = 3L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "between 0 and 3")

  err <- rlang::catch_cnd(check_integer(0, lb = 0, inclusive = FALSE,
                                        name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "greater than 0")

  expect_equal(check_integer(5, name = "check1"), 5L)
  expect_equal(check_integer(5L, name = "check1"), 5L)
  expect_equal(check_integer(0, lb = 0, inclusive = TRUE, name = "check1"), 0L)
  expect_equal(check_integer(6, lb = 0, name = "check1"), 6L)
})

test_that("check_character", {
  err <- rlang::catch_cnd(check_character(1L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "character scalar")
  expect_equal(err$not, "integer")

  err <- rlang::catch_cnd(check_character(letters[1:5], name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "length 1")
  expect_equal(err$not, 5L)

  err <- rlang::catch_cnd(check_character(NA_character_, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "non-missing")

  expect_equal(check_character("intercept", name = "check1"), "intercept")
  expect_equal(check_character(NA, allow_na = TRUE, name = "check1"),
               NA_character_)
  expect_equal(check_character(NA_character_, allow_na = TRUE, name = "check1"),
               NA_character_)
})
