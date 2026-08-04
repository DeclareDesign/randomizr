library(randomizr)

# prob_ra guarantees three things at once, and each is a per-draw property, so
# every draw is a test of the first two. The marginals need repetition.
#
#   1. every unit gets exactly one condition
#   2. each unit's probability of each condition is exactly as supplied
#   3. each condition's count is the floor or the ceiling of its expected count

props <- function(Z_mat, P, blocks = NULL) {
  k <- ncol(P)
  tgt <- if (is.null(blocks)) matrix(colSums(P), nrow = 1) else
    sapply(seq_len(k), function(j) tapply(P[, j], factor(blocks), sum))
  if (is.null(dim(tgt))) tgt <- matrix(tgt, nrow = 1)
  tight <- apply(Z_mat, 2, function(z) {
    got <- if (is.null(blocks)) matrix(sapply(seq_len(k), function(j) sum(z == j)), nrow = 1)
           else sapply(seq_len(k), function(j) tapply(z == j, factor(blocks), sum))
    if (is.null(dim(got))) got <- matrix(got, nrow = 1)
    all(got >= floor(tgt) - 1e-8 & got <= ceiling(tgt) + 1e-8)
  })
  list(tight = all(tight),
       marg = sapply(seq_len(k), function(j) rowMeans(Z_mat == j)))
}

draw_int <- function(P, blocks = NULL, S = 2000) {
  cond <- seq_len(ncol(P))
  # matrix() rather than bare replicate(): with a single unit, replicate drops
  # the dimension and returns a vector, which silently breaks apply() below.
  matrix(replicate(S, as.integer(as.character(
    prob_ra(prob_unit_each = P, blocks = blocks, conditions = cond)))),
    nrow = nrow(P))
}

test_that("two arms with unit-varying probabilities: exact marginals, fixed total", {
  set.seed(1)
  p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)     # sums to 3
  r <- replicate(3000, prob_ra(prob_unit = p))
  expect_true(all(r %in% c(0, 1)))
  expect_true(all(colSums(r) == 3))         # tight, and the sum is an integer
  expect_equal(rowMeans(r), p, tolerance = 0.03)
})

test_that("a scalar prob_unit works when N or blocks says how many units", {
  set.seed(1)
  expect_length(prob_ra(prob_unit = 0.5, N = 10), 10)
  expect_length(prob_ra(prob_unit = 0.5, blocks = rep(1:2, each = 5)), 10)
  # the call that errored in the original probra
  expect_error(prob_ra(prob_unit = 0.5), "supply `N` or `blocks`")
})

test_that("blocked assignment is tight within every block", {
  set.seed(2)
  # Macartan's example: two districts of three, three treated, target 1.5 each.
  districts <- rep(c("north", "south"), each = 3)
  r <- replicate(2000, prob_ra(prob_unit = rep(0.5, 6), blocks = districts))
  north <- colSums(r[districts == "north", ])
  south <- colSums(r[districts == "south", ])
  expect_true(all(north %in% 1:2))          # never 0, never 3
  expect_true(all(south %in% 1:2))
  expect_equal(rowMeans(r), rep(0.5, 6), tolerance = 0.04)
})

test_that("multi-arm counts are tight, which the sequential method could not do", {
  set.seed(3)
  # The case Macartan flagged as problematic. Arm 2 expects 1.13 units, so it
  # must always receive 1 or 2. The original prob_ra returned 0 about 12% of
  # the time.
  P <- cbind(c(.15, .47), c(.65, .48), c(.20, .05))
  Z <- draw_int(P, S = 3000)
  expect_true(all(colSums(Z == 2) %in% 1:2))
  pr <- props(Z, P)
  expect_true(pr$tight)
  expect_equal(pr$marg, P, tolerance = 0.03)
})

test_that("every unit receives exactly one condition, over adversarial inputs", {
  set.seed(4)
  for (rep in 1:60) {
    n <- sample(1:15, 1); k <- sample(2:5, 1)
    P <- switch(sample(1:3, 1),
      { m <- matrix(stats::rexp(n * k), n, k); m / rowSums(m) },
      matrix(1 / k, n, k),
      { m <- matrix(0, n, k)                       # some rows exactly degenerate
        for (i in seq_len(n)) m[i, sample(k, 1)] <- 1
        if (n > 1) { i <- sample(n, 1); w <- stats::runif(k); m[i, ] <- w / sum(w) }
        m })
    Z <- draw_int(P, S = 4)
    expect_true(all(Z >= 1 & Z <= k))
    expect_true(props(Z, P)$tight)
  }
})

test_that("blocked multi-arm is tight per block and per arm at once", {
  set.seed(5)
  for (rep in 1:25) {
    B <- sample(2:4, 1); sizes <- sample(2:6, B, replace = TRUE)
    blocks <- rep(seq_len(B), sizes); n <- length(blocks); k <- sample(2:4, 1)
    m <- matrix(stats::rexp(n * k), n, k); P <- m / rowSums(m)
    Z <- draw_int(P, blocks = blocks, S = 4)
    expect_true(props(Z, P, blocks)$tight)
  }
})

test_that("invalid probabilities are refused, which probra accepted", {
  expect_error(prob_ra(prob_unit = c(-0.5, 0.5, 1)), "between 0 and 1")
  expect_error(prob_ra(prob_unit = c(0.5, 2, 0.5)), "between 0 and 1")
  expect_error(prob_ra(prob_unit = c(0.5, NA, 0.5)), "must not be missing")
  expect_error(prob_ra(prob_unit_each = cbind(c(.5, .5), c(.2, .2))), "sum to 1")
  expect_error(prob_ra(), "Supply either")
  expect_error(prob_ra(prob_unit = 0.5, prob_unit_each = matrix(.5, 2, 2)),
               "only one of")
  expect_error(prob_ra(prob_unit = c(.5, .5), blocks = c(1, 2, 3)), "`blocks` has length")
})

test_that("conditions are named and typed like the rest of the package", {
  set.seed(6)
  z <- prob_ra(prob_unit = rep(0.5, 6))
  expect_true(is.numeric(z))
  expect_setequal(unique(z), c(0, 1))
  z2 <- prob_ra(prob_unit = rep(0.5, 6), conditions = c("control", "treatment"))
  expect_true(all(as.character(z2) %in% c("control", "treatment")))
  P <- matrix(1/3, 6, 3)
  z3 <- prob_ra(prob_unit_each = P)
  expect_true(all(as.character(z3) %in% c("T1", "T2", "T3")))
})

test_that("prob_ra_probabilities returns the supplied probabilities", {
  p <- c(0.2, 0.4, 0.6)
  M <- prob_ra_probabilities(prob_unit = p)
  expect_equal(colnames(M), c("prob_0", "prob_1"))
  expect_equal(M[, "prob_1"], p)
  expect_equal(rowSums(M), rep(1, 3))
})
