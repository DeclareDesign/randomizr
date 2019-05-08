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

test_that("Weighted rs", {
  
  
  p1 <- simple_rs_probabilities(N = 4, prob_unit = 1:4 / 4)

  expect_equal(p1, 1:4/4)
  
  draws <- replicate(100, simple_rs(N=5, prob_unit = 0:4/4))
  expect_true(all(draws[1,] == 0))
  expect_true(all(draws[5,] == 1))
  
})

test_that("prob_unit",{
  
  simple_rs(100, prob_unit = seq(0.1, 0.99, length.out = 100))
  expect_error(simple_rs(100, prob = seq(0.1, 0.99, length.out = 100)))
  
})

