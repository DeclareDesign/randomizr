library(randomizr)

# Cube-on-X is a separate C++ entry. These tests cover the formula path.
# The count-tight blocks path is unchanged; leftover pairing lives in
# test_balanced_ra.R.

test_that("formula = ~ x on the toy vector tracks the x total and the marginals", {
  set.seed(41)
  x <- c(0, 1, 5, 6, 8, 9)
  p <- rep(0.5, 6)
  r <- replicate(2000, balanced_ra(prob_unit = p, formula = ~ x))
  r_cnt <- replicate(2000, balanced_ra(prob_unit = p))
  expect_true(all(r %in% 0:1))
  expect_equal(rowMeans(r), p, tolerance = 0.04)
  expect_equal(mean(colSums(x * r)), sum(x * p), tolerance = 0.4)
  expect_true(all(colSums(r) == 3))
  # Landing may miss 14.5; it still beats count-only on the x-total.
  rmse <- function(Z) sqrt(mean((colSums(x * (Z - p)))^2))
  expect_true(rmse(r) < rmse(r_cnt))
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

test_that("formula looks up covariates in the calling environment", {
  x <- rnorm(20)
  z <- balanced_ra(N = 20, formula = ~ x)
  expect_length(z, 20)
  expect_true(all(z %in% 0:1))
  z2 <- local({
    x <- 1:10
    balanced_ra(N = 10, formula = ~ x)
  })
  expect_length(z2, 10)
  z3 <- balanced_ra(formula = ~ x)
  expect_length(z3, 20)
})

test_that("N is inferred from formula when omitted", {
  x <- 1:12
  z <- balanced_ra(formula = ~ x)
  expect_length(z, 12)
  expect_true(all(z %in% 0:1))
  expect_length(balanced_ra(N = 12, formula = ~ x), 12)
  expect_error(
    balanced_ra(N = 6, formula = ~ x),
    "produces 12 rows"
  )
})

test_that("formula still accepts an explicit data frame", {
  x <- rnorm(20)
  z <- balanced_ra(N = 20, formula = ~ x, data = data.frame(x = x))
  expect_length(z, 20)
  expect_true(all(z %in% 0:1))
})

test_that("missing formula variable errors", {
  expect_error(
    balanced_ra(N = 20, formula = ~ this_var_is_not_defined_zz),
    "this_var_is_not_defined_zz|calling environment|not found"
  )
})

test_that("length of the model matrix must match N", {
  x <- c(0, 1, 5)
  expect_error(
    balanced_ra(N = 6, formula = ~ x),
    "produces 3 rows"
  )
})

test_that("DeclareDesignZero finds formula vars without data=", {
  skip_if_not_installed("fabricatrZero")
  skip_if_not_installed("DeclareDesignZero")
  pop <- DeclareDesignZero::declare_model(N = 10, x = 1:N)
  des <- pop + DeclareDesignZero::declare_assignment(
    Z = balanced_ra(formula = ~ x)
  )
  dat <- DeclareDesignZero::draw_data(des)
  expect_equal(nrow(dat), 10)
  expect_true(all(dat$Z %in% 0:1))
})

test_that("formula works with data and heterogeneous p", {
  set.seed(43)
  dat <- data.frame(x = c(0, 1, 5, 6, 8, 9),
                    B = factor(rep(c("a", "b"), each = 3)))
  p <- c(0.2, 0.3, 0.4, 0.6, 0.7, 0.8)
  r <- replicate(1500, balanced_ra(prob_unit = p, formula = ~ x + B, data = dat))
  r_sim <- replicate(1500, simple_ra(N = 6, prob_unit = p))
  expect_equal(rowMeans(r), p, tolerance = 0.05)
  rmse <- function(Z) sqrt(mean((colSums(dat$x * (Z - p)))^2))
  expect_true(rmse(r) < rmse(r_sim))
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
