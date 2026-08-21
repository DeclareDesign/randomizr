# randomizr 1.x reproducibility.
#
# 2.0 moved two-arm blocked assignment into C++. That is invisible to the API
# but not to the random number stream, and for a randomization package a seed
# that stops reproducing breaks every pre-registration and replication script
# that pinned one. The strings below are the output of randomizr 1.0.1,
# produced by running 1.0.1 itself, so a future change to any RNG path fails
# here rather than being discovered by a user.
#
# Regenerate ONLY against a real 1.0.1 install, never against the current build.

b3   <- rep(c("a", "b", "c"), each = 20)
bodd <- rep(c("a", "b"), each = 15)
bun  <- rep(c("a", "b", "c"), times = c(7, 13, 30))
clu  <- rep(1:15, each = 4)
b5   <- rep(seq_len(6), each = 5)
b10  <- rep(seq_len(6), each = 10)
bodd3 <- rep(c("a", "b", "c"), each = 15)
m3   <- rbind(c(10, 5, 5), c(4, 8, 8), c(7, 7, 6))
p3   <- rbind(c(0.2, 0.3, 0.5), c(1/3, 1/3, 1/3), c(0.5, 0.25, 0.25))

expect_stream <- function(code, expected) {
  set.seed(1)
  expect_equal(paste(as.character(code), collapse = ""), expected)
}

test_that("block_ra reproduces randomizr 1.0.1", {
  expect_stream(block_ra(blocks = b3),   "000011111010010110100011100110000101101100110001110111101000")
  expect_stream(block_ra(blocks = bodd), "000011010110111001111101000010")
  expect_stream(block_ra(blocks = bun),  "10010111000100111101001011000000011101111011001101")
  expect_stream(block_ra(blocks = b3, prob = 0.4), "000011011010010100100011100100000101101000010001110110101000")
})

test_that("cluster_ra and strata_rs reproduce randomizr 1.0.1", {
  expect_stream(cluster_ra(clusters = clu), "000000000000000011111111000011110000111111110000111111110000")
  expect_stream(strata_rs(strata = b3),     "000011111010010110100011100110000101101100110001110111101000")
})

test_that("the treated counts are exactly what complete randomization requires", {
  # Guards against a stream that reproduces by accident while assigning wrongly.
  set.seed(1); z <- block_ra(blocks = b3)
  expect_equal(as.integer(tapply(z, b3, sum)), c(10L, 10L, 10L))
  set.seed(1); z <- cluster_ra(clusters = clu)
  expect_equal(length(unique(tapply(z, clu, function(v) length(unique(v))))), 1L)
})

# Both C++ kernels compute floor(n * prob) and then n * prob - floor(n * prob).
# Written inline the compiler contracts those into an FMA, the product never
# rounds to a double, and the leftover comes out as 4e-16 where R gets exactly
# 0. It is far too small to see in any probability and it still moves the
# assignment, because an exact tie and a near-tie resolve to different arms.
# The cases below are the ones that caught it: they are not decorative, and a
# fix-up value that stops being exact will fail here rather than in someone's
# replication script.
test_that("floor and remainder are computed the way R computes them", {
  expect_stream(block_ra(blocks = b5, prob = 0.1),
                "000010000000000001001000000001")
  expect_stream(block_ra(blocks = b10, prob = 0.35),
                "010000111000001100011100101000000001011001100010001000010010")
  expect_stream(block_ra(blocks = b10, prob = 0.45),
                "010000111100011100011110101000000101011001110010001001010010")
  expect_stream(block_ra(blocks = bodd3, prob = 0.1),
                "000010000000100001000000000000001100000000000")
})

# Three or more arms, and two arms reached through prob_each or m_each, run in
# block_assign_multi_cpp(). Same requirement as above: 1.0.1's draw, not merely
# 1.0.1's distribution. Recorded from 1.0.1, including the tie cases, where the
# arm receiving a leftover unit is decided by revsort()'s unstable descending
# sort and no independent implementation lands on it by accident.
test_that("multi-arm blocked assignment reproduces randomizr 1.0.1", {
  expect_stream(block_ra(blocks = b3, num_arms = 3),
                "T2T1T1T2T3T3T3T1T2T1T3T1T2T3T2T2T2T1T3T3T1T2T2T3T1T1T1T1T3T3T3T3T2T2T1T2T3T3T2T1T1T2T2T2T2T3T1T3T3T3T2T1T1T3T3T1T2T3T2T1")
  expect_stream(block_ra(blocks = bodd3, num_arms = 3),
                "T2T1T2T1T1T3T3T1T2T3T1T2T3T2T3T2T2T2T1T3T3T3T2T3T3T1T2T1T1T1T3T1T1T1T2T2T3T2T3T2T2T1T3T3T1")
  expect_stream(block_ra(blocks = bun, num_arms = 4),
                "T1T2T3T3T4T4T1T1T1T2T4T2T3T1T4T4T3T2T2T3T1T2T4T2T3T1T3T1T1T3T3T4T4T4T2T1T3T3T2T2T3T3T2T1T4T4T1T1T2T4")
  expect_stream(block_ra(blocks = b3, prob_each = c(0.2, 0.3, 0.5)),
                "T1T2T1T1T3T3T3T3T3T1T3T2T2T3T2T3T3T2T3T2T2T2T3T3T3T1T2T3T3T1T1T1T2T3T2T3T3T2T3T3T2T2T3T3T2T2T1T3T3T3T1T3T3T3T3T1T3T1T2T2")
  expect_stream(block_ra(blocks = bodd3, prob_each = c(0.2, 0.3, 0.5)),
                "T2T2T1T1T3T3T1T3T2T3T3T2T2T3T3T3T2T3T3T3T3T3T1T3T2T1T2T1T3T2T2T3T3T2T2T3T3T2T3T1T3T3T1T1T2")
  expect_stream(block_ra(blocks = b3, prob_each = c(1/3, 1/3, 1/3)),
                "T2T1T1T2T3T3T3T1T2T1T3T1T2T3T2T2T2T1T3T3T1T2T2T3T1T1T1T1T3T3T3T3T2T2T1T2T3T3T2T1T1T2T2T2T2T3T1T3T3T3T2T1T1T3T3T1T2T3T2T1")
  expect_stream(block_ra(blocks = b3, block_m_each = m3),
                "T1T1T1T1T2T3T2T3T2T1T3T1T1T3T1T2T2T1T3T1T2T2T3T3T3T1T2T3T2T1T1T1T2T3T2T3T3T2T3T2T1T2T2T3T2T1T1T3T3T3T1T2T3T2T3T1T2T1T2T1")
  expect_stream(block_ra(blocks = b3, block_prob_each = p3),
                "T1T2T1T1T3T3T3T3T3T1T3T2T2T3T2T3T3T2T3T2T3T1T2T1T2T3T2T2T1T1T1T1T3T2T3T2T3T2T3T3T1T1T2T3T1T1T1T3T3T3T1T2T3T2T2T1T2T1T1T1")
  expect_stream(block_ra(blocks = bodd3, block_prob_each = p3),
                "T2T2T1T1T3T3T1T3T2T3T3T2T2T3T3T2T1T3T3T3T3T2T1T2T2T1T1T1T3T2T2T1T1T1T3T2T2T1T3T1T3T2T1T1T1")
  expect_stream(block_ra(blocks = b3, prob_each = c(0.3, 0.7)),
                "010011111010110111110111101110000111111101111101110111101010")
})
