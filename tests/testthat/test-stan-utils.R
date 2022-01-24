test_that("model matrix is renamed", {
  expect_equal(model_matrix_name_repair(c("(taylor)", "Swift", "all:too:well")),
               c("taylor", "swift", "all__too__well"))
  expect_equal(
    model_matrix_name_repair(c("(Intercept)", "att1", "att2", "att1:att2")),
    c("intercept", "att1", "att2", "att1__att2")
  )
})

test_that("can identify component parameters", {
  # main effects have no component parameters
  expect_identical(one_down_params(x = "1", item = 12), "")
  expect_identical(one_down_params(x = "5", item = 10), "")
  expect_identical(one_down_params(x = "3", item = 8), "")
  expect_identical(one_down_params(x = "54", item = 7), "")

  # two-way interactions have main effect components
  expect_identical(one_down_params(x = "1__2", item = 3), "l3_11,l3_12")
  expect_identical(one_down_params(x = "5__6", item = 10), "l10_15,l10_16")
  expect_identical(one_down_params(x = "3__4", item = 6), "l6_13,l6_14")
  expect_identical(one_down_params(x = "9__10", item = 3), "l3_19,l3_110")

  # three-way interactions have two-way interaction components
  expect_identical(one_down_params(x = "1__2__3", item = 1),
                   "l1_212,l1_213,l1_223")
  expect_identical(one_down_params(x = "5__9__12", item = 22),
                   "l22_259,l22_2512,l22_2912")

  # four-way interactions have three-way interaction components
  expect_identical(one_down_params(x = "1__2__3__4", item = 1),
                   "l1_3123,l1_3124,l1_3134,l1_3234")
  expect_identical(one_down_params(x = "2__4__6__8", item = 13),
                   "l13_3246,l13_3248,l13_3268,l13_3468")
})
