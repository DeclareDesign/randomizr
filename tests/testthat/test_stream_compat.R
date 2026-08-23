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

# The fixtures above pinned only the blocked hot path. The ones below extend
# the pinning to every family that reproduces 1.0.1: complete, simple,
# clustered, blocked-and-clustered, the m / block_m / block_prob / prob_unit /
# m_unit forms of block_ra (the two-arm kernel's modes 0 and 2), and the
# sampling functions. All recorded from a real 1.0.1 install. bfull's first
# block is fully treated, which is the case where 1.x's complete_ra() returns
# before touching the RNG: a kernel that draws a permutation there produces
# the right distribution and the wrong stream.

bfull <- rep(c("A", "B", "C"), c(3, 4, 5))

test_that("block_ra modes 0 and 2 reproduce randomizr 1.0.1", {
  expect_stream(block_ra(blocks = b3, m = 5),
                "000001010010010000100001100100000001100000010001110010000000")
  expect_stream(block_ra(blocks = bfull, m = 3),
                "111011111100")
  expect_stream(block_ra(blocks = bun, block_m = c(3, 6, 20)),
                "00101010100011001101001111001100011101111011111101")
  expect_stream(block_ra(blocks = bun, block_m = c(7, 13, 30)),
                "11111111111111111111111111111111111111111111111111")
  expect_stream(block_ra(blocks = bun, block_m = c(0, 6, 30)),
                "00000000100011001101111111111111111111111111111111")
  expect_stream(block_ra(blocks = b3, m_unit = rep(c(5, 10, 15), each = 20)),
                "000001010010010000100011100110000101101111111101110111101010")
  expect_stream(block_ra(blocks = b3, block_prob = c(0.2, 0.5, 0.8)),
                "000001010010000000100101001110000101101111111101110111101011")
  expect_stream(block_ra(blocks = bodd3, block_prob = c(0.3, 0.5, 0.9)),
                "000011010000100101111101000010111111111011101")
  expect_stream(block_ra(blocks = b3, prob_unit = rep(c(0.2, 0.5, 0.8), each = 20)),
                "000001010010000000100101001110000101101111111101110111101011")
})

test_that("complete_ra reproduces randomizr 1.0.1", {
  expect_stream(complete_ra(N = 21),
                "000001110101001100111")
  expect_stream(complete_ra(N = 21, m = 7),
                "000000110101001000101")
  expect_stream(complete_ra(N = 21, m = 21),
                "111111111111111111111")
  expect_stream(complete_ra(N = 21, prob = 0.3),
                "000000110101000000101")
  expect_stream(complete_ra(N = 21, m_each = c(5, 6, 10)),
                "T1T2T1T1T2T3T3T3T1T3T1T3T2T2T3T3T2T2T3T3T3")
  expect_stream(complete_ra(N = 21, prob_each = c(0.2, 0.3, 0.5)),
                "T1T2T1T1T3T3T3T3T1T3T2T3T2T2T3T3T2T2T3T3T3")
  expect_stream(complete_ra(N = 21, num_arms = 3),
                "T1T1T1T1T2T2T3T3T1T3T1T3T1T2T3T2T2T2T3T2T3")
  expect_stream(complete_ra(N = 21, conditions = c("x", "y", "z")),
                "xxxxyyzzxzxzxyzyyyzyz")
})

test_that("simple_ra reproduces randomizr 1.0.1", {
  expect_stream(simple_ra(N = 21, prob = 0.4),
                "000101111000101011011")
  expect_stream(simple_ra(N = 21, prob_each = c(0.2, 0.3, 0.5)),
                "T2T2T3T3T2T3T3T3T3T1T2T1T3T2T3T2T3T3T2T3T3")
  expect_stream(simple_ra(N = 21, prob_unit = seq(0.05, 1, length.out = 21)),
                "000101111000111111111")
})

test_that("cluster_ra and block_and_cluster_ra reproduce randomizr 1.0.1", {
  expect_stream(cluster_ra(clusters = clu, m = 6),
                "000000000000000000001111111100000000111100000000111111111111")
  expect_stream(cluster_ra(clusters = clu, prob = 0.35),
                "000000000000000011111111000011110000111100000000111100000000")
  expect_stream(cluster_ra(clusters = clu, m_each = c(4, 5, 6)),
                "T2T2T2T2T1T1T1T1T2T2T2T2T1T1T1T1T1T1T1T1T3T3T3T3T3T3T3T3T1T1T1T1T2T2T2T2T3T3T3T3T2T2T2T2T2T2T2T2T3T3T3T3T3T3T3T3T3T3T3T3")
  expect_stream(cluster_ra(clusters = clu, num_arms = 3),
                "T2T2T2T2T1T1T1T1T2T2T2T2T1T1T1T1T1T1T1T1T3T3T3T3T3T3T3T3T1T1T1T1T2T2T2T2T3T3T3T3T1T1T1T1T2T2T2T2T3T3T3T3T2T2T2T2T3T3T3T3")
  expect_stream(block_and_cluster_ra(blocks = b3, clusters = clu),
                "111111110000000011111111000011110000000000000000000011111111")
  expect_stream(block_and_cluster_ra(blocks = b3, clusters = clu,
                                     block_m_each = rbind(c(2, 3), c(1, 4), c(3, 2))),
                "000011111111111100001111111111111111000000001111000011110000")
})

test_that("the sampling functions reproduce randomizr 1.0.1", {
  expect_stream(complete_rs(N = 21),
                "000011110101001100111")
  expect_stream(complete_rs(N = 21, n = 7),
                "000000110101001000101")
  expect_stream(simple_rs(N = 21, prob = 0.3),
                "000101100000001011011")
  expect_stream(cluster_rs(clusters = clu),
                "000000000000000011111111000011110000111111110000111111111111")
  expect_stream(cluster_rs(clusters = clu, n = 6),
                "000000000000000000001111111100000000111100000000111111111111")
})

test_that("the RNG stream position matches 1.0.1 across a sequence of draws", {
  # A draw can coincide while the stream has drifted, and then everything
  # AFTER it in a replication script silently changes. Pinning the stream
  # position after several draws catches that; the fully treated first block
  # of bfull is here because 1.x consumed no RNG for it.
  set.seed(42)
  z1 <- block_ra(blocks = bfull, m = 3)
  z2 <- complete_ra(N = 21, prob = 0.3)
  z3 <- cluster_ra(clusters = clu, num_arms = 3)
  expect_equal(paste(z1, collapse = ""), "111011101110")
  expect_equal(paste(z2, collapse = ""), "100001000001110011000")
  expect_equal(paste(as.character(z3), collapse = ""),
               "T2T2T2T2T2T2T2T2T3T3T3T3T2T2T2T2T3T3T3T3T3T3T3T3T1T1T1T1T2T2T2T2T3T3T3T3T1T1T1T1T1T1T1T1T3T3T3T3T1T1T1T1T2T2T2T2T1T1T1T1")
  expect_equal(runif(1), 0.038936491124332, tolerance = 1e-12)
})

test_that("strata_rs returns what block_ra returns, per NEWS", {
  # The one documented exception to 1.x reproducibility, stated as the exact
  # rule it follows rather than only on the strata where it happens to agree.
  s <- rep(c("a", "b", "c"), c(15, 8, 21))
  set.seed(3); a <- block_ra(blocks = s, prob = 0.4)
  set.seed(3); b <- strata_rs(strata = s, prob = 0.4)
  expect_identical(as.integer(a), as.integer(b))
  set.seed(4); a <- block_ra(blocks = s, block_prob = c(0.2, 0.5, 0.9))
  set.seed(4); b <- strata_rs(strata = s, strata_prob = c(0.2, 0.5, 0.9))
  expect_identical(as.integer(a), as.integer(b))
  blk <- rep(c("a", "b"), each = 14); cl2 <- rep(1:14, each = 2)
  set.seed(5); a <- block_and_cluster_ra(blocks = blk, clusters = cl2)
  set.seed(5); b <- strata_and_cluster_rs(strata = blk, clusters = cl2)
  expect_identical(as.integer(a), as.integer(b))
})

test_that("balanced_ra draws are pinned to their current stream", {
  # Not a 1.x comparison (balanced_ra is new); a self-pin, so the next
  # refactor of the cube kernels cannot move every balanced assignment
  # silently the way the flight-phase rewrite could have.
  expect_stream(balanced_ra(prob_unit = c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)),
                "010101")
  expect_stream(balanced_ra(blocks = rep(c("n", "s"), each = 3)),
                "001011")
  expect_stream(balanced_ra(prob_unit_each = matrix(c(0.2, 0.4, 0.6, 0.8,
                                                      0.4, 0.3, 0.2, 0.1,
                                                      0.4, 0.3, 0.2, 0.1), 4, 3)),
                "T2T3T1T1")
  x_pin <- c(0, 1, 5, 6, 8, 9)
  expect_stream(balanced_ra(N = 6, formula = ~ x_pin),
                "100110")
})
