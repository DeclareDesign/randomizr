context("Tricky Examples")

# Compare tricky examples vs. randomizr

#devtools::install_github("acoppock/randomizr")

# Examples ----------------------------------------------------------------

B <- rep(2:4,9)

# Checks out
table(B, block_ra(blocks = B, prob_each = rep(1/3,3)))

# randomizr doesn't rescale
expect_error(table(B, block_ra(blocks = B, prob_each = c(.33,.33,.33))))
table(B, block_ra(blocks = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33))))

# Works
B <- c("A", "B", "A", "D")
table(B, block_ra(blocks = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33))))

B <- c("A", "B", "D")
table(B, block_ra(blocks = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33))))

table(c(B,B), block_ra(blocks= c(B,B), prob_each=c(.43,.33,.33)/sum(c(.43,.33,.33))))


B = c(1,2,1,2,1)
table(B, block_ra(blocks = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33))))

# Complete random assignment for factorial
table(complete_ra(16))

table(complete_ra(16, prob = .25))


# Complete random assignment into 4 categories eg for factorial
n <- 16
table(complete_ra(16, prob_each = rep(.25, 4), condition_names = c("T00", "T01", "T10", "T11")))


# Block examples
B <- c("A","A","B","B")

table(B, block_ra(blocks = B))


# More complex examples
B <- c(1,1,2,2)
table(block_ra(blocks =B, prob_each=c(.21,.29,.5)))
# NOTE: both procedures sometimes have no units in a condition!


# Global balance even if within block balance not possible
B <- c(1,1,1,2,2,2)
table(B, block_ra(blocks = B, prob = .5))

B <- c(1,1,1,2,2,2,2)
table(B, block_ra(blocks = B, prob = .5))

B <- c(1,1,1,2,2,2,2,2,2)
table(B, block_ra(blocks = B, prob_each =c(1/3,1/3,1/3)))

B <- c(1,1,1,2,2,2,2,2,2, 3, 4, 4)

table(B, block_ra(blocks = B, prob_each =c(1/6,1/6,1/6, 1/2), 
                  condition_names = c("A", "B", "C", "D")))


# Bonus trick to show balancing with block_prob_each

blocks <- rep(c("A", "B","C"), times=c(51, 103, 207))

block_prob_each <- rbind(c(.3, .6, .1),
                         c(.2, .7, .1),
                         c(.1, .8, .1))


table(blocks, block_ra(blocks, block_prob_each = block_prob_each))



