library(testthat)
library(randomizr)

context("Complete Random Sampling")


# Errors ------------------------------------------------------------------

expect_error(complete_rs(N=100, m=101))
expect_error(complete_rs(N=100, m=-2))
expect_error(complete_rs(N=0, m=0))
expect_error(complete_rs(N=1, m=2))



S <- complete_rs(N = 100)
expect_equal(sum(S), 50)

probs <- complete_rs_probabilities(N = 100)
expect_true(all.equal(probs, rep(0.5, 100)))

S <- complete_rs(100, m = 30)
expect_equal(sum(S), 30)

probs <- complete_rs_probabilities(N = 100, m = 30)
expect_true(all.equal(probs, rep(0.3, 100)))



S <- complete_rs(N=101, prob = .34)
probs <- complete_rs_probabilities(N = 101, prob = .34)
expect_true(all.equal(probs, rep(0.34, 101)))


# Special Cases -----------------------------------------------------------

expect_lt(sum(replicate(1000, complete_rs(1))), 600)
expect_gt(sum(replicate(1000, complete_rs(1))), 400)
# correct!
replicate(100, complete_rs(N=1, m=1))

expect_equal(sum(replicate(100, complete_rs(N=1, m=0))), 0)
expect_equal(sum(replicate(100, complete_rs(N=2, m=0))), 0)
expect_equal(sum(replicate(100, complete_rs(N=3, m=0))), 0)
expect_equal(complete_rs(N=2, m=2), c(1,1))
