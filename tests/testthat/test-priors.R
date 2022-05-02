test_that("measrprior works", {
  prior1 <- measrprior("normal(0, 10)", class = "intercept")
  check1 <- tibble::tibble(class = "intercept", coef = NA_character_,
                 prior_def = "normal(0, 10)")
  expect_s3_class(prior1, "measrprior")
  expect_identical(unclass(prior1), unclass(check1))

  prior2 <- measrprior("lognormal(0, 1)", class = "maineffect")
  check2 <- tibble::tibble(class = "maineffect", coef = NA_character_,
                           prior_def = "lognormal(0, 1)")
  expect_s3_class(prior2, "measrprior")
  expect_identical(unclass(prior2), unclass(check2))

  prior3 <- measrprior("normal(0, 2)", coef = "l12_212", class = "interaction")
  check3 <- tibble::tibble(class = "interaction", coef = "l12_212",
                           prior_def = "normal(0, 2)")
  expect_s3_class(prior3, "measrprior")
  expect_identical(unclass(prior3), unclass(check3))

  prior4 <- measrprior("normal(0, 2)", class = "intercept", lb = "-3.0",
                       ub = "0.5")
  check4 <- tibble::tibble(class = "intercept", coef = NA_character_,
                           prior_def = "normal(0, 2)T[-3.0,0.5]")
  expect_s3_class(prior4, "measrprior")
  expect_identical(unclass(prior4), unclass(check4))
})

test_that("alias functions work", {
  prior1 <- prior(normal(0, 10), class = intercept)
  prior2 <- prior_(~normal(0, 10), class = ~intercept)
  prior3 <- prior_string("normal(0, 10)", class = "intercept")
  expect_identical(prior1, prior2)
  expect_identical(prior1, prior3)
  expect_identical(prior2, prior3)

  prior1 <- prior(beta(5, 17), coef = l3_11, lb = 0.1, ub = 0.3,
                  class = maineffect)
  prior2 <- prior_(~beta(5, 17), coef = ~l3_11, lb = ~0.1, ub = ~0.3,
                   class = ~maineffect)
  prior3 <- prior_string("beta(5, 17)", coef = "l3_11", lb = "0.1", ub = "0.3",
                         class = "maineffect")
  expect_identical(prior1, prior2)
  expect_identical(prior1, prior3)
  expect_identical(prior2, prior3)
})

test_that("mixing types works", {
  prior1 <- prior(lognormal(0, 1), class = "intercept", lb = -2.5, ub = 0.5)
  prior2 <- prior_(~lognormal(0, 1), class = "intercept", lb = -2.5, ub = 0.5)
  prior3 <- prior_string("lognormal(0, 1)", class = "intercept",
                         lb = "-2.5", ub = "0.5")
  expect_identical(prior1, prior2)
  expect_identical(prior1, prior3)
  expect_identical(prior2, prior3)
})

test_that("validator works", {
  err <- rlang::catch_cnd(
    validate_measrprior(new_measrprior(
      tibble::tibble(class = "intercept", prior_def = "normal(0, 10)")
    ))
  )
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "x")
  expect_match(err$message, "Missing variables: coef")

  err <- rlang::catch_cnd(
    validate_measrprior(new_measrprior(
      tibble::tibble(prior_def = "normal(0, 10)")
    ))
  )
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "x")
  expect_match(err$message, "Missing variables: class, coef")

  err <- rlang::catch_cnd(
    validate_measrprior(new_measrprior(
      tibble::tibble(class = "intercept", coef = NA_character_,
                     prior_def = "normal(0, 10)", my_var = "blue")
    ))
  )
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "x")
  expect_match(err$message, "Extra variables: my_var")

  err <- rlang::catch_cnd(
    validate_measrprior(new_measrprior(
      tibble::tibble(class = "intercept", my_var2 = "pur", coef = NA_character_,
                     prior_def = "normal(0, 10)", my_var = "blue")
    ))
  )
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "x")
  expect_match(err$message, "Extra variables: my_var2, my_var")

  err <- rlang::catch_cnd(
    validate_measrprior(new_measrprior(
      tibble::tibble(class = NA_character_, coef = NA_character_,
                     prior_def = "normal(0, 10)")
    ))
  )
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "x")
  expect_match(err$message, "not contain missing values")

  err <- rlang::catch_cnd(
    validate_measrprior(new_measrprior(
      tibble::tibble(class = "intercept", coef = NA_character_,
                     prior_def = NA_character_)
    ))
  )
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "x")
  expect_match(err$message, "not contain missing values")

  err <- rlang::catch_cnd(
    validate_measrprior(new_measrprior(
      tibble::tibble(class = NA_character_, coef = NA_character_,
                     prior_def = NA_character_)
    ))
  )
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "x")
  expect_match(err$message, "not contain missing values")

  err <- rlang::catch_cnd(
    validate_measrprior(new_measrprior(
      tibble::tibble(class = "structural", coef = NA_character_,
                     prior_def = "normal(0, 10)")
    ))
  )
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "x")
  expect_match(err$message, "only include values of .* for prior class")
})

test_that("class check works", {
  prior1 <- prior(normal(0, 1))
  expect_true(is.measrprior(prior1))

  prior2 <- prior_string("beta(5,17)", class = "maineffect")
  expect_true(is.measrprior(prior2))

  prior3 <- prior_(~lognormal(0, 1), class = "interaction", coef = "l1_212")
  expect_true(is.measrprior(prior3))

  expect_false(is.measrprior(3))
  expect_false(is.measrprior("normal(0, 1)"))
  expect_false(is.measrprior(tibble::tibble()))
  expect_false(is.measrprior(tibble::tibble(class = "intercept",
                                            coef = NA_character_,
                                            param_def = "normal(0, 1)")))
})

test_that("default priors", {
  # lcdm defaults
  expect_equal(unclass(default_dcm_priors(type = "lcdm")),
               list(class = c("intercept", "maineffect", "interaction"),
                    coef = rep(NA_character_, 3),
                    prior_def = c("normal(0, 15)", "lognormal(0, 1)",
                                  "normal(0, 15)")),
               ignore_attr = TRUE)

  # dina defaults
  expect_equal(unclass(default_dcm_priors(type = "dina")),
               list(class = c("slip", "guess"),
                    coef = rep(NA_character_, 2),
                    prior_def = c("beta(5, 25)", "beta(5, 25)")),
               ignore_attr = TRUE)

  # dino defaults
  expect_equal(unclass(default_dcm_priors(type = "dino")),
               list(class = c("slip", "guess"),
                    coef = rep(NA_character_, 2),
                    prior_def = c("beta(5, 25)", "beta(5, 25)")),
               ignore_attr = TRUE)
})

test_that("priors can be combined", {
  prior1 <- prior(normal(13, 22), class = "intercept")
  prior2 <- prior(lognormal(0, 1), class = "maineffect")
  combined_prior <- c(prior1, prior2)
  expect_s3_class(combined_prior, "measrprior")
  expect_equal(unclass(combined_prior),
               list(class = c("intercept", "maineffect"),
                    coef = c(NA_character_, NA_character_),
                    prior_def = c("normal(13, 22)", "lognormal(0, 1)")),
               ignore_attr = TRUE)

  user_priors <- c(prior(normal(-2, 3), class = intercept),
                   prior(lognormal(0, 5), class = maineffect),
                   prior(lognormal(0, 0.2), class = maineffect, coef = l1_12))
  final_priors <- c(user_priors, default_dcm_priors(type = "lcdm"),
                    replace = TRUE)
  expect_s3_class(user_priors, "measrprior")
  expect_s3_class(final_priors, "measrprior")
  expect_equal(unclass(final_priors),
               list(class = c("intercept", "maineffect", "maineffect",
                              "interaction"),
                    coef = c(NA_character_, NA_character_, "l1_12",
                             NA_character_),
                    prior_def = c("normal(-2, 3)", "lognormal(0, 5)",
                                  "lognormal(0, 0.2)", "normal(0, 15)")),
               ignore_attr = TRUE)

  rmv_class <- class(user_priors)[!(class(user_priors) == "measrprior")]
  class(user_priors) <- rmv_class
  err <- rlang::catch_cnd(c(final_priors, user_priors))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "Objects")
  expect_match(err$message, "`measrprior` objects")
  expect_equal(err$not, "tbl_df")

  err <- rlang::catch_cnd(c(final_priors, final_priors, user_priors))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "Objects")
  expect_match(err$message, "`measrprior` objects")
  expect_equal(err$not, "tbl_df")

  expect_equal(final_priors, c(final_priors))
})
