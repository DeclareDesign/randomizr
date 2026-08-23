# Findings of the 2026-08-23 synoptic review, each pinned so it stays fixed.

test_that("an NA in blocks or clusters is an error on every path", {
  bl_na <- c(1, 1, NA, 2, 2, 2)
  expect_error(block_ra(bl_na), "must not contain NA")
  expect_error(block_ra(bl_na, num_arms = 3), "must not contain NA")
  expect_error(block_ra(bl_na, prob_each = c(0.3, 0.3, 0.4)), "must not contain NA")
  expect_error(block_ra(bl_na, block_m_each = rbind(c(1, 1), c(1, 2))), "must not contain NA")
  expect_error(block_ra_probabilities(bl_na), "must not contain NA")
  expect_error(declare_ra(blocks = bl_na), "must not contain NA")
  expect_error(strata_rs(strata = bl_na), "must not contain NA")
  expect_error(cluster_ra(clusters = bl_na), "must not contain NA")
  expect_error(cluster_rs(clusters = bl_na), "must not contain NA")
  expect_error(block_and_cluster_ra(blocks = c(1, 1, NA, 2), clusters = c(1, 1, 2, 2)),
               "must not contain NA")
  expect_error(balanced_ra(prob = 0.5, blocks = c(NA, NA, 1, 1, 2, 2)), "must not contain NA")
  expect_error(balanced_ra(prob = 0.5, clusters = c(1, 1, NA, 2, 2, NA)), "must not contain NA")
  expect_error(balanced_ra(prob_unit_each = matrix(1/3, 6, 3), blocks = c(1, 1, NA, 2, 2, 2)),
               "must not contain NA")
  # and with checking off, the kernels still refuse rather than crash
  expect_error(block_ra(bl_na, num_arms = 3, conditions = c("a", "b", "c"),
                        check_inputs = FALSE), "must not contain NA")
  expect_error(balanced_ra(prob_unit = rep(0.5, 6), blocks = bl_na,
                           check_inputs = FALSE), "must not contain NA")
})

test_that("check_inputs = FALSE cannot reach unsafe memory in the multi-arm kernel", {
  blocks <- rep(1:2, each = 10)
  for (i in 1:25) {
    expect_error(block_ra(blocks, prob_each = c(0.1, 0.1), check_inputs = FALSE),
                 "sum to 1")
    expect_error(block_ra(rep(1, 200), prob_each = c(1e-7, 1e-7), check_inputs = FALSE),
                 "sum to 1")
    expect_error(block_ra(blocks, block_m_each = rbind(c(99, 1), c(5, 5)),
                          check_inputs = FALSE), "counts sum to")
  }
  expect_error(block_ra(rep(1:3, 5), block_prob = c(0.5, 0.5), check_inputs = FALSE),
               "2 block probabilities were supplied for 3 blocks")
  expect_error(balanced_ra(prob_unit = rep(0.5, 4), blocks = c(1, 1, 2),
                           check_inputs = FALSE), "length 3")
})

test_that("a blocked declaration made with check_inputs = FALSE can be conducted", {
  bl <- rep(c("A", "B", "C"), c(3, 4, 5))
  d <- declare_ra(blocks = bl, check_inputs = FALSE)
  set.seed(7); z1 <- conduct_ra(d)
  set.seed(7); z2 <- block_ra(blocks = bl)
  expect_identical(z1, z2)
  d2 <- declare_ra(blocks = bl, prob_each = c(0.2, 0.3, 0.5), check_inputs = FALSE)
  expect_length(conduct_ra(d2), 12L)
  d3 <- declare_ra(blocks = bl, block_m_each = rbind(c(1, 2), c(2, 2), c(3, 2)),
                   check_inputs = FALSE)
  expect_length(conduct_ra(d3), 12L)
})

test_that("unused factor levels are dropped, not half-supported", {
  f <- factor(c("a", "a", "b", "b", "b"), levels = c("a", "b", "c"))
  fc <- factor(c("a", "a", "b", "b", "b"))
  d <- declare_ra(blocks = f)
  Z <- conduct_ra(d)
  expect_length(Z, 5L)
  expect_equal(nrow(d$probabilities_matrix), 5L)
  expect_equal(obtain_condition_probabilities(d, Z), rep(0.5, 5))
  expect_equal(nrow(block_ra_probabilities(f)), 5L)
  expect_length(strata_rs(strata = f), 5L)
  # an unused middle level behaves like an unused last level
  fm <- factor(c("a", "a", "c", "c", "c"), levels = c("a", "b", "c"))
  set.seed(2); zm <- block_ra(fm, m = 1)
  expect_equal(as.integer(tapply(zm, as.character(fm), sum)), c(1L, 1L))
  # and a full-factor draw equals the same subset drawn as characters
  set.seed(9); z1 <- block_ra(f)
  set.seed(9); z2 <- block_ra(fc)
  expect_identical(z1, z2)
})

test_that("declarations do not retain data", {
  dat <- data.frame(bl = rep(1:2, 500))
  dat$junk <- rnorm(1000)
  d_data <- declare_ra(blocks = bl, data = dat)
  d_plain <- declare_ra(blocks = dat$bl)
  size_data <- length(serialize(d_data, NULL))
  size_plain <- length(serialize(d_plain, NULL))
  # the two declarations describe the same design, so they should weigh the
  # same up to small bookkeeping differences, not by the size of dat
  expect_lt(size_data, size_plain + 20000)
  set.seed(11); z1 <- conduct_ra(d_data)
  set.seed(11); z2 <- conduct_ra(d_plain)
  expect_identical(as.integer(z1), as.integer(z2))
})

test_that("formula = ~ 1 is complete assignment, not a deterministic pairing", {
  set.seed(31)
  M <- replicate(600, balanced_ra(N = 6, formula = ~ 1))
  expect_true(all(colSums(M) == 3))
  agree <- mean(M[1, ] == M[2, ])
  # complete assignment of 3 of 6 gives P(agree) = 0.4; the bug gave exactly 0
  expect_gt(agree, 0.25)
  expect_lt(agree, 0.55)
})

test_that("an NA in a formula covariate is an error, not a shorter assignment", {
  x_na <- c(1, 2, NA, 4)
  expect_error(balanced_ra(formula = ~ x_na), "finite and non-missing")
  expect_error(declare_ra(formula = ~ x_na), "finite and non-missing")
})

test_that("data resolves through a wrapper's dots", {
  dat <- data.frame(bl = c(1, 1, 2, 2))
  k <- function(d, ...) conduct_ra(data = d, ...)
  expect_length(k(dat, blocks = bl), 4L)
  k2 <- function(...) declare_ra(...)
  d <- k2(blocks = bl, data = dat)
  expect_length(conduct_ra(d), 4L)
})

test_that("declare_ra(data = ) checks N against the data", {
  dat <- data.frame(bl = c(1, 1, 2, 2))
  expect_error(declare_ra(N = 5, blocks = bl, data = dat), "N should equal")
})

test_that("balanced_ra follows complete_ra's naming and type conventions", {
  expect_type(balanced_ra(6), "integer")
  expect_s3_class(balanced_ra(6, num_arms = 2), "factor")
  expect_equal(levels(balanced_ra(6, num_arms = 2)), c("T1", "T2"))
  z <- balanced_ra(prob_unit_each = matrix(0.5, 6, 2))
  expect_type(z, "integer")
  expect_true(all(z %in% 0:1))
  # num_arms and conditions without probabilities expand, as in complete_ra
  z3 <- balanced_ra(6, num_arms = 3)
  expect_equal(as.vector(table(z3)), c(2L, 2L, 2L))
  z3n <- balanced_ra(6, conditions = c("a", "b", "c"))
  expect_true(all(z3n %in% c("a", "b", "c")))
  p3 <- balanced_ra_probabilities(6, num_arms = 3)
  expect_equal(unname(p3), matrix(1/3, 6, 3), ignore_attr = TRUE)
  expect_equal(colnames(balanced_ra_probabilities(prob_unit_each = matrix(0.5, 4, 2))),
               c("prob_0", "prob_1"))
})

test_that("balanced_ra refuses a non-numeric N", {
  expect_error(balanced_ra(N = "4"), "single positive integer")
})

test_that("cluster_rs accepts prob_unit and n_unit end to end", {
  cl <- rep(1:12, each = 5)
  expect_silent(S <- cluster_rs(clusters = cl, prob_unit = rep(0.3, 60)))
  expect_length(S, 60L)
  expect_equal(unname(cluster_rs_probabilities(clusters = cl, prob_unit = rep(0.3, 60))),
               rep(0.3, 60))
  expect_equal(unname(cluster_rs_probabilities(clusters = cl, n_unit = rep(4, 60))),
               rep(4 / 12, 60))
  d <- declare_rs(clusters = cl, n_unit = rep(4, 60))
  expect_equal(unname(d$probabilities_matrix[, 2]), rep(1 / 3, 60))
  expect_output(print(d))
  expect_equal(unname(obtain_inclusion_probabilities(d)), rep(1 / 3, 60))
})

test_that("balanced_ra marginals are exact, not approximate", {
  set.seed(97)
  n <- 20000
  p <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  r <- replicate(n, balanced_ra(prob_unit = p))
  z <- (rowMeans(r) - p) / sqrt(p * (1 - p) / n)
  expect_true(all(abs(z) < 4.5))
  x_z <- c(0, 1, 5, 6, 8, 9)
  rf <- replicate(n, balanced_ra(prob_unit = c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5),
                                 formula = ~ x_z))
  pf <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
  zf <- (rowMeans(rf) - pf) / sqrt(pf * (1 - pf) / n)
  expect_true(all(abs(zf) < 4.5))
})

test_that("RS declarations enumerate their permutations", {
  d <- declare_rs(strata = rep(c("a", "b"), each = 4))
  pm <- obtain_permutation_matrix(d)
  expect_equal(ncol(pm), obtain_num_permutations(d))
  expect_equal(ncol(unique(pm, MARGIN = 2)), ncol(pm))
  d2 <- declare_rs(clusters = rep(1:4, each = 2))
  expect_equal(ncol(obtain_permutation_matrix(d2)), obtain_num_permutations(d2))
  d3 <- declare_rs(N = 6, n = 3)
  expect_equal(ncol(obtain_permutation_matrix(d3)), choose(6, 3))
  d4 <- declare_rs(N = 958, n = 479)
  expect_equal(dim(obtain_permutation_matrix(d4)), c(958L, 10000L))
})
