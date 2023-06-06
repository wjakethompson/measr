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

  # three-way interactions have one- and two-way interaction components
  expect_identical(one_down_params(x = "1__2__3", item = 1),
                   paste0("l1_11+l1_212+l1_213,",
                          "l1_12+l1_212+l1_223,",
                          "l1_13+l1_213+l1_223"))
  expect_identical(one_down_params(x = "5__9__12", item = 22),
                   paste0("l22_15+l22_259+l22_2512,",
                          "l22_19+l22_259+l22_2912,",
                          "l22_112+l22_2512+l22_2912"))

  # four-way interactions have one-, two-, and three-way interaction components
  expect_identical(one_down_params(x = "1__2__3__4", item = 1),
                   paste0(
                     "l1_11+l1_212+l1_213+l1_214+l1_3123+l1_3124+l1_3134,",
                     "l1_12+l1_212+l1_223+l1_224+l1_3123+l1_3124+l1_3234,",
                     "l1_13+l1_213+l1_223+l1_234+l1_3123+l1_3134+l1_3234,",
                     "l1_14+l1_214+l1_224+l1_234+l1_3124+l1_3134+l1_3234"
                   ))
  expect_identical(one_down_params(x = "2__4__6__8", item = 9),
                   paste0(
                     "l9_12+l9_224+l9_226+l9_228+l9_3246+l9_3248+l9_3268,",
                     "l9_14+l9_224+l9_246+l9_248+l9_3246+l9_3248+l9_3468,",
                     "l9_16+l9_226+l9_246+l9_268+l9_3246+l9_3268+l9_3468,",
                     "l9_18+l9_228+l9_248+l9_268+l9_3248+l9_3268+l9_3468"
                   ))
})
