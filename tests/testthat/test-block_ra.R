library(testthat)
library(randomizr)

context("Block Random Assignments")

block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
#block_var <- factor(block_var, levels = c("B", "A", "C"))
#block_var <- rep(1:3, times=c(50, 100, 200))


Z <- block_ra(block_var=block_var)
table(block_var, Z)

Z <- block_ra(block_var=block_var, block_m = c(20, 30, 40))

table(block_var, Z)



block_m_each <- rbind(c(25, 25),
                      c(50, 50),
                      c(100, 100))

Z <- block_ra(block_var=block_var, block_m_each=block_m_each)
table(block_var, Z)

block_m_each <- rbind(c(10, 40),
                      c(30, 70),
                      c(50, 150))

Z <- block_ra(block_var=block_var, block_m_each=block_m_each, 
              condition_names=c("control", "treatment"))
table(block_var, Z)

# Multi-arm Designs
Z <- block_ra(block_var=block_var, num_arms=2)
table(block_var, Z)
Z <- block_ra(block_var=block_var, num_arms=3)
table(Z)
table(block_var, Z)

Z <- block_ra(block_var=block_var, num_arms=3, balance_load = TRUE)
table(Z)
table(block_var, Z)



Z <- block_ra(block_var=block_var, num_arms=4)
table(block_var, Z)
Z <- block_ra(block_var=block_var, num_arms=5)
table(block_var, Z)
Z <- block_ra(block_var=block_var, num_arms=6)
table(block_var, Z)

block_m_each <- rbind(c(10, 20, 20),
                      c(30, 50, 20),
                      c(50, 75, 75))
Z <- block_ra(block_var=block_var, block_m_each=block_m_each )
table(block_var, Z)

Z <- block_ra(block_var=block_var, block_m_each=block_m_each, 
              condition_names=c("control", "placebo", "treatment"))
table(block_var, Z)

Z <- block_ra(block_var=block_var, prob_each=c(.1, .1, .8))
table(block_var, Z)
Z <- block_ra(block_var=block_var, prob_each=c(.31, .48, .21))
table(block_var, Z)
Z <- block_ra(block_var=block_var, prob_each=c(.213, .568, .219))
table(block_var, Z)


Z <- block_ra(block_var = block_var, prob = .5)
table(block_var,Z)
Z <- block_ra(block_var = block_var, prob = 1)
table(block_var,Z)
Z <- block_ra(block_var = block_var, prob = 0)
table(block_var,Z)
Z <- block_ra(block_var = block_var, prob = .33)
table(block_var,Z)


block_var <- rep(c("A", "B","C"), times=c(51, 103, 207))

block_prob_each <- rbind(c(.3, .6, .1),
                         c(.2, .7, .1),
                         c(.1, .8, .1))

table(block_var, block_ra(block_var, block_prob_each = block_prob_each))




# Macartan's worry: with blocks of size 3, can assign either 1 or 2 to control. How to fix total number of assignments

block_var <- rep(c("A", "B","C"), times=c(3, 3, 3))
expect_true(all(replicate(100, sum(block_ra(block_var = block_var, balance_load = TRUE))) %in% c(5,4)))
expect_false(all(replicate(100, sum(block_ra(block_var = block_var))) %in% c(5,4)))



# Confirming Errors Correctly Thrown --------------------------------------


block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))

block_m_each <- rbind(c(25, 25),
                      c(50, 50),
                      c(100, 100))

block_prob_each <- rbind(c(.3, .6, .1),
                         c(.2, .7, .1),
                         c(.1, .8, .1))




expect_error(block_ra(block_var=block_var, block_m_each=block_m_each, block_prob_each = block_prob_each))
expect_error(block_ra(block_var=block_var, block_m_each=block_m_each, prob_each = c(.2, .1, .7)))
expect_error(block_ra(block_var=block_var, block_prob_each = block_prob_each, prob_each = c(.2, .1, .7)))

expect_error(block_ra(block_var=block_var, num_arms = 2, block_prob_each = block_prob_each))
expect_error(block_ra(block_var=block_var, num_arms = 2, prob_each = c(.2, .1, .7)))
expect_error(block_ra(block_var=block_var, num_arms = 3, block_m_each=block_m_each))

expect_error(block_ra(block_var=block_var, condition_names = c("1", "2"), block_prob_each = block_prob_each))
expect_error(block_ra(block_var=block_var, condition_names = c("1", "2"), prob_each = c(.2, .1, .7)))
expect_error(block_ra(block_var=block_var, condition_names = c("1", "2", "3"), block_m_each=block_m_each))

expect_error(block_ra(block_var=block_var, condition_names = c("1", "2", "3"), num_arms = 2))

