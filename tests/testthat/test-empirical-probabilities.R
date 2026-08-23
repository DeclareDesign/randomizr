context("Empirical probabilities")

# The remainder step of complete_ra() draws the leftover units with
# probability proportional to the fractional parts, which is exactly what
# keeps each unit's marginal probability equal to prob_each even when a block
# is too small for the floors to allocate anything. This file asserts that
# the reported probabilities are prob_each and that empirical frequencies
# agree, on a design whose blocks are small enough (3, 2, 1) to make the
# floors nearly empty.

test_that("reported and empirical probabilities agree on tiny blocks", {
  blocks <- c("A", "A", "A", "B", "B", "C")
  prob_each <- c(0.1, 0.2, 0.7)

  pm <- block_ra_probabilities(blocks = blocks, prob_each = prob_each)
  expect_equal(unname(pm), matrix(prob_each, 6, 3, byrow = TRUE),
               ignore_attr = TRUE)

  set.seed(42)
  sims <- 4000
  counts <- matrix(0, 6, 3)
  for (i in seq_len(sims)) {
    z <- block_ra(blocks = blocks, prob_each = prob_each)
    for (j in 1:3) counts[, j] <- counts[, j] + (z == paste0("T", j))
  }
  emp <- counts / sims
  z_stat <- (emp - pm) / sqrt(pm * (1 - pm) / sims)
  expect_true(all(abs(z_stat) < 4.5))
})
