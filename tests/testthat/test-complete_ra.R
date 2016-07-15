library(testthat)
library(randomizr)

context("Complete Random Assignments")


expect_error(complete_ra(100, prob_each = c(.2)))
expect_error(complete_ra(100, m_each = c(30, 70), prob_each = c(.3, .7)))
expect_error(complete_ra(100, m_each=c(20, 20, 20)))
expect_error(complete_ra(100, m_each=c(20, 20, 60), condition_names=c(1,2)))
expect_error(complete_ra(100, prob_each = c(.2, .7)))
expect_error(complete_ra(N=100, m=101))
expect_error(complete_ra(N=100, m=-2))
expect_error(complete_ra(N=0, m=0))
expect_error(complete_ra(N=1, m=2))

table(complete_ra(100, m=30, condition_names = c("Control", "Treatment")))

# should this be allowed?
complete_ra(100, num_arms = 1)
complete_ra(100, m_each = c(100))
complete_ra(100, condition_names = c("Treatment"))


table(complete_ra(1, num_arms = 2))
table(complete_ra(1, num_arms = 2, condition_names = c(0,1)))

Z <- complete_ra(N=100)
expect_equal(sum(Z), 50)

Z <- complete_ra(N=101, prob = .34); table(Z)

Z <- complete_ra(N=100, m=50)
expect_equal(sum(Z), 50)

Z <- complete_ra(N=100, m_each = c(30, 70), 
                 condition_names = c("control", "treatment"))
table(Z)

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

complete_ra(2, m_each = c(1, 0, 1), condition_names = c("T1", "T2", "T3"))


expect_lt(sum(replicate(1000, complete_ra(1))), 600)
expect_gt(sum(replicate(1000, complete_ra(1))), 400)
# correct!
replicate(100, complete_ra(N=1, m=1))

expect_equal(sum(replicate(100, complete_ra(N=1, m=0))), 0)
expect_equal(sum(replicate(100, complete_ra(N=2, m=0))), 0)
expect_equal(sum(replicate(100, complete_ra(N=3, m=0))), 0)

expect_equal(complete_ra(N=2, m=2), c(1,1))


Z <- complete_ra(N=100, m_each=c(50, 50, 0))
expect_equivalent(as.numeric(table(Z)), c(50, 50, 0))

Z <- complete_ra(N=100, m_each=c(100, 0, 0))
expect_equivalent(as.numeric(table(Z)), c(100, 0, 0))
