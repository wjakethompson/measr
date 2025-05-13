test_that("methods classes work", {
  expect_true(S7::S7_inherits(stanmethod))

  expect_true(S7::S7_inherits(stanmethod(), stanmethod))
  expect_true(S7::S7_inherits(mcmc(), stanmethod))
  expect_true(S7::S7_inherits(optim(), stanmethod))
  expect_true(S7::S7_inherits(gqs(), stanmethod))
})

test_that("backend methods work", {
  expect_true(S7::S7_inherits(stanbackend))

  expect_true(S7::S7_inherits(stanbackend(), stanbackend))
  expect_true(S7::S7_inherits(rstan(), stanbackend))
  expect_true(S7::S7_inherits(cmdstanr(), stanbackend))
})
