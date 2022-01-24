test_that("deparsing works", {
  expect_identical(deparse_combine(quote(normal(0, 1))), "normal(0, 1)")
  expect_identical(deparse_combine(quote(normal(0, 1)), max_char = 3),
                   "nor")

  expect_identical(as_string(1), "1")
  expect_identical(as_string(~normal(0, 1)), "normal(0, 1)")
  expect_identical(as_string("normal"), "normal")

  err <- rlang::catch_cnd(as_string(`+`))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "Arguments")
  expect_match(err$message, "one-sided formula, call, name, or constant")
})

test_that("named lists work", {
  expect_identical(named_list(taylor_swift = 13L,
                              beyonce = 4L,
                              all_too_well = "10 minute version"),
                   list(taylor_swift = 13L,
                        beyonce = 4L,
                        all_too_well = "10 minute version"))

  expect_identical(named_list("bill_self",
                              ~jayhawk(0, 1),
                              champions = c(1922L, 1923L, 1952L, 1988L, 2008L)),
                   list(bill_self = "bill_self",
                        `~jayhawk(0, 1)` = ~jayhawk(0, 1),
                        champions = c(1922L, 1923L, 1952L, 1988L, 2008L)))

  expect_identical(named_list("andy reid", "patrick-mahomes", "travis_kelce"),
                   list(`andy reid` = "andy reid",
                        `patrick-mahomes` = "patrick-mahomes",
                        travis_kelce = "travis_kelce"))
})
