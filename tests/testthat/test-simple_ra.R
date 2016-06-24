library(testthat)
library(randomizr)

context("Simple Random Assignments")

# Two Group Designs
Z <- simple_ra(N=100)
table(Z)

Z <- simple_ra(N=100, prob=0.5)
table(Z)

Z <- simple_ra(N=100, prob_each = c(0.3, 0.7), 
               condition_names = c("control", "treatment"))
table(Z)

# Multi-arm Designs
Z <- simple_ra(N=100, num_arms=3)
table(Z)

Z <- simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4))
table(Z)

Z <- simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4), 
               condition_names=c("control", "placebo", "treatment"))
table(Z)

Z <- simple_ra(N=100, condition_names=c("control", "placebo", "treatment"))
table(Z)


# Special Cases

replicate(100, simple_ra(N=1, prob = 0))
replicate(100, simple_ra(N=1, prob = 1))
replicate(100, simple_ra(N=3, prob = 0))
replicate(100, simple_ra(N=3, prob = 1))

replicate(100, simple_ra(N=1, prob_each = c(0, 1)))
replicate(100, simple_ra(N=1, prob_each = c(0, 0, 1)))
replicate(100, simple_ra(N=3, prob_each = c(0, 1)))
replicate(100, simple_ra(N=4, prob_each = c(0, 0, 1)))



