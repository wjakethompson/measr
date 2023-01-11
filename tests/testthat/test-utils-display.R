test_that("paste_line", {
  example1 <- c("Kansas", "Missouri", "South Carolina", "Utah")

  expect_equal(paste_line(example1), "Kansas\nMissouri\nSouth Carolina\nUtah")
  expect_equal(paste_line(example1, .trailing = TRUE),
               "Kansas\nMissouri\nSouth Carolina\nUtah\n")
})
