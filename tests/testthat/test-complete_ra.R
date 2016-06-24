library(testthat)
library(randomizr)

context("Complete Random Assignments")

expect_error(complete_ra(100, num_arms = 1))
expect_error(complete_ra(100, condition_names = c("Treatment")))
expect_error(complete_ra(100, m_each = c(100)))
expect_error(complete_ra(100, prob_each = c(.2)))
expect_error(complete_ra(100, m=50, condition_names = c("Control", "Treatment")))
expect_error(complete_ra(100, m_each = c(30, 70), prob_each = c(.3, .7)))
expect_error(complete_ra(2, 2))
expect_error(complete_ra(100, m_each=c(20, 20, 20)))
expect_error(complete_ra(100, m_each=c(20, 20, 60), condition_names=c(1,2)))
expect_error(complete_ra(100, prob_each = c(.2, .7)))

Z <- complete_ra(N=100)
expect_equal(sum(Z), 50)

Z <- complete_ra(N=101, prob = .34); table(Z)

Z <- complete_ra(N=100, m=50)
expect_equal(sum(Z), 50)

Z <- complete_ra(N=100, m_each = c(30, 70), 
                 condition_names = c("control", "treatment"))

expect_equivalent(as.numeric(table(Z)), c(30, 70))

# Multi-arm Designs
Z <- complete_ra(N=100, num_arms=3)

expect_true(all(table(Z) %in% c(33, 34)))

Z <- complete_ra(N=100, m_each=c(30, 30, 40))

expect_equivalent(as.numeric(table(Z)), c(30, 30, 40))

Z <- complete_ra(N=100, m_each=c(30, 30, 40), 
                 condition_names=c("control", "placebo", "treatment"))
expect_equivalent(as.numeric(table(Z)), c(30, 30, 40))

Z <- complete_ra(N=100, condition_names=c("control", "placebo", "treatment"))

expect_true(all(table(Z) %in% c(33, 34)))

# Special Cases

expect_less_than(sum(replicate(1000, complete_ra(1))), 600)
expect_more_than(sum(replicate(1000, complete_ra(1))), 400)

