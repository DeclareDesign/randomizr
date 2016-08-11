library(testthat)
library(randomizr)

context("Simple Random Sampling")

# Two Group Designs
S <- simple_ra(N = 100)
table(S)

probs <- simple_rs_probabilities(N = 100)
table(probs)

S <- simple_ra(N = 100, prob = 0.1)
table(S)

probs <- simple_rs_probabilities(N = 100, prob = 0.1)
table(probs)
