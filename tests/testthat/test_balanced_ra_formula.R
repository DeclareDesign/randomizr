library(randomizr)

# Cube-on-X is a separate C++ entry. These tests cover the formula path.
# The count-tight blocks path is unchanged; leftover pairing lives in
# test_balanced_ra.R.

test_that("formula = ~ x on the toy vector concentrates the x total", {
  set.seed(41)
  x <- c(0, 1, 5, 6, 8, 9)
  p <- rep(0.5, 6)
  r <- replicate(2000, balanced_ra(prob_unit = p, formula = ~ x))
  expect_true(all(r %in% 0:1))
  expect_equal(rowMeans(r), p, tolerance = 0.04)
  # Target sum(x p) = 14.5 is not integer; landing sits on 14 or 15.
  sx <- colSums(x * r)
  expect_true(mean(sx %in% 14:15) > 0.85)
  expect_true(all(colSums(r) %in% 2:4))
})

test_that("~ 0 + x lets the treated count wander", {
  set.seed(42)
  x <- c(0, 1, 5, 6, 8, 9)
  r <- replicate(800, balanced_ra(prob_unit = 0.5, N = 6, formula = ~ 0 + x))
  expect_equal(rowMeans(r), rep(0.5, 6), tolerance = 0.06)
  # No intercept: the count need not stay at 3.
  expect_true(any(colSums(r) != 3))
})

test_that("formula and blocks together error", {
  x <- c(0, 1, 5, 6, 8, 9)
  expect_error(
    balanced_ra(prob_unit = 0.5, formula = ~ x, blocks = rep(1:3, each = 2)),
    "Use B in the formula, or use blocks=, not both"
  )
})

test_that("formula and prob_unit_each together error", {
  x <- c(0, 1)
  P <- matrix(1 / 3, 2, 3)
  expect_error(
    balanced_ra(prob_unit_each = P, formula = ~ x),
    "not yet supported with `prob_unit_each`"
  )
})

test_that("length of the model matrix must match N", {
  x <- c(0, 1, 5)
  expect_error(
    balanced_ra(N = 6, formula = ~ x),
    "produces 3 rows"
  )
})

test_that("formula works with data and heterogeneous p", {
  set.seed(43)
  dat <- data.frame(x = c(0, 1, 5, 6, 8, 9),
                    B = factor(rep(c("a", "b"), each = 3)))
  p <- c(0.2, 0.3, 0.4, 0.6, 0.7, 0.8)
  r <- replicate(1500, balanced_ra(prob_unit = p, formula = ~ x + B, data = dat))
  expect_equal(rowMeans(r), p, tolerance = 0.05)
  T_x <- colSums(dat$x * (r - p))
  expect_true(sqrt(mean(T_x^2)) < 2)
})

test_that("formula at the cluster-collapsed level keeps clusters together", {
  set.seed(44)
  clusters <- rep(1:6, each = 2)
  x <- rep(c(0, 1, 5, 6, 8, 9), each = 2)
  z <- balanced_ra(prob_unit = 0.5, formula = ~ x, clusters = clusters)
  expect_true(all(tapply(z, clusters, function(v) length(unique(v)) == 1)))
  expect_length(z, 12)
})

test_that("~ 1 matches count-tight unblocked assignment on totals", {
  set.seed(45)
  p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
  r <- replicate(400, balanced_ra(prob_unit = p, formula = ~ 1))
  expect_true(all(colSums(r) == 3))
  expect_equal(rowMeans(r), p, tolerance = 0.08)
})
