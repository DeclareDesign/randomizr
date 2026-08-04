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
