library(testthat)
library(randomizr)

context("Simple Random Sampling")

test_that("simple_ra works", {
  # Two Group Designs
  S <- simple_rs(N = 100)
  expect_true(all(S %in% 0:1))
})

test_that("SRS works", {
    
  probs <- simple_rs_probabilities(N = 100)
  expect_true(all(probs == .5))
})

test_that("SRS  .1", {
    
  S <- simple_rs(N = 100, prob = 0.1)
  expect_true(all(S %in% 0:1))
})

test_that("SRS probs .1", {
  
  probs <- simple_rs_probabilities(N = 100, prob = 0.1)
  expect_true(all(probs == .1))
})