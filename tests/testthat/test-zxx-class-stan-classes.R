test_that("methods classes work", {
  expect_s7_class(stanmethod, S7::S7_object)

  expect_s7_class(stanmethod(), stanmethod)
  expect_s7_class(mcmc(), stanmethod)
  expect_s7_class(optim(), stanmethod)
  expect_s7_class(gqs(), stanmethod)
})

test_that("backend methods work", {
  expect_s7_class(stanbackend, S7::S7_object)

  expect_s7_class(stanbackend(), stanbackend)
  expect_s7_class(rstan(), stanbackend)
  expect_s7_class(cmdstanr(), stanbackend)
})
