context("Declarations: the data argument")

make_dat <- function() {
  data.frame(
    bl = rep(c("a", "b"), each = 10),
    cl = rep(1:10, each = 2),
    x = seq(-1, 1, length.out = 20),
    stringsAsFactors = FALSE
  )
}

test_that("blocks and clusters name columns of data", {
  dat <- make_dat()

  d <- declare_ra(blocks = bl, data = dat)
  expect_equal(class(d)[2], "ra_blocked")
  expect_equal(d$blocks, dat$bl)
  expect_equal(d$N, nrow(dat))

  d2 <- declare_ra(blocks = bl, clusters = cl, data = dat)
  expect_equal(class(d2)[2], "ra_blocked_and_clustered")
  expect_equal(d2$clusters, dat$cl)

  d3 <- declare_ra(clusters = cl, data = dat)
  expect_equal(class(d3)[2], "ra_clustered")
})

test_that("a column name given as a string works too", {
  dat <- make_dat()
  expect_equal(declare_ra(blocks = "bl", data = dat)$blocks, dat$bl)
  expect_equal(declare_ra(clusters = "cl", data = dat)$clusters, dat$cl)
})

test_that("an expression is allowed when every variable in it is a column", {
  dat <- make_dat()
  d <- declare_ra(blocks = interaction(bl, cl %% 2), data = dat)
  expect_equal(as.character(d$blocks), as.character(interaction(dat$bl, dat$cl %% 2)))
})

test_that("data supplies the formula's variables", {
  dat <- make_dat()
  d <- declare_ra(formula = ~ x, data = dat)
  expect_equal(d$N, nrow(dat))
  expect_equal(unname(d$.X[, 2]), dat$x)
})

test_that("a variable absent from data is an error, not a fall-through", {
  dat <- make_dat()
  nope <- rep("z", nrow(dat))

  expect_error(declare_ra(blocks = nope, data = dat), "does not have")
  expect_error(declare_ra(clusters = nope, data = dat), "does not have")
  expect_error(declare_ra(formula = ~ nope, data = dat), "does not have")

  # A path through an object in the environment names that object, not a
  # column, so it is refused: with `data` you write blocks = bl.
  expect_error(declare_ra(blocks = dat$bl, data = dat), "does not have")
})

test_that("data wins over a same-named object in the calling environment", {
  dat <- make_dat()
  bl <- rep("decoy", nrow(dat))
  x <- rep(99, nrow(dat))

  expect_equal(declare_ra(blocks = bl, data = dat)$blocks, dat$bl)
  expect_equal(unname(declare_ra(formula = ~ x, data = dat)$.X[, 2]), dat$x)
})

test_that("a declaration written in one frame resolves against data, not the stack", {
  dat <- make_dat()
  declare_here <- function() declare_ra(formula = ~ x, data = dat)
  d <- declare_here()

  x <- rep(99, nrow(dat))  # visible at conduct time; must not matter
  expect_equal(unname(d$.X[, 2]), dat$x)
  expect_length(conduct_ra(d), nrow(dat))
})

test_that("a column of the wrong length is refused", {
  dat <- make_dat()
  expect_error(declare_ra(blocks = bl[1:5], data = dat), "but `data` has")
})

test_that("data is not stored in the declaration", {
  dat <- make_dat()
  d <- declare_ra(blocks = bl, data = dat)
  expect_false("data" %in% ls(d))
})

test_that("conduct_ra and obtain_condition_probabilities take data inline", {
  dat <- make_dat()

  set.seed(11)
  Z <- conduct_ra(blocks = bl, data = dat)
  expect_length(Z, nrow(dat))
  expect_equal(as.vector(table(dat$bl, Z)), c(5, 5, 5, 5))

  p <- obtain_condition_probabilities(assignment = Z, blocks = bl, data = dat)
  expect_equal(unique(p), 0.5)

  Zf <- conduct_ra(formula = ~ x, data = dat)
  expect_length(Zf, nrow(dat))
})

test_that("without data, lookup in the calling environment is unchanged", {
  dat <- make_dat()
  bl <- dat$bl
  x <- dat$x

  expect_equal(class(declare_ra(blocks = bl))[2], "ra_blocked")
  expect_equal(declare_ra(blocks = bl)$blocks, dat$bl)
  expect_equal(unname(declare_ra(formula = ~ x)$.X[, 2]), dat$x)
})

test_that("every per-unit argument resolves against data", {
  dat <- make_dat()
  dat$p <- seq(0.3, 0.7, length.out = nrow(dat))
  dat$mu <- rep(10, nrow(dat))
  dat$pa <- dat$p
  dat$pb <- 1 - dat$p

  d <- declare_ra(prob_unit = p, ra_type = "balanced", data = dat)
  expect_equal(class(d)[2], "ra_balanced")
  expect_equal(d$prob_unit, dat$p)

  d2 <- declare_ra(m_unit = mu, data = dat)
  expect_equal(class(d2)[2], "ra_complete")
  expect_equal(d2$m_unit, dat$mu)

  d3 <- declare_ra(prob_unit_each = cbind(pa, pb), data = dat)
  expect_equal(class(d3)[2], "ra_balanced")
  expect_equal(dim(d3$prob_unit_each), c(nrow(dat), 2L))
  expect_equal(unname(d3$prob_unit_each[, 1]), dat$pa)

  expect_equal(declare_ra(prob_unit = "p", ra_type = "balanced", data = dat)$prob_unit,
               dat$p)
})

test_that("a per-unit argument floating in the environment is refused", {
  dat <- make_dat()
  dat$p <- seq(0.3, 0.7, length.out = nrow(dat))
  dat$pa <- dat$p
  p_local <- rep(0.5, nrow(dat))
  mu_local <- rep(10, nrow(dat))

  expect_error(declare_ra(prob_unit = p_local, ra_type = "balanced", data = dat),
               "does not have")
  expect_error(declare_ra(m_unit = mu_local, data = dat), "does not have")
  expect_error(declare_ra(prob_unit_each = cbind(pa, nope), data = dat),
               "does not have")
})

test_that("data wins over a same-named per-unit object in the caller", {
  dat <- make_dat()
  dat$p <- seq(0.3, 0.7, length.out = nrow(dat))
  p <- rep(0.99, nrow(dat))

  expect_equal(declare_ra(prob_unit = p, ra_type = "balanced", data = dat)$prob_unit,
               dat$p)
})

test_that("a scalar in a per-unit slot is redirected to the scalar slot", {
  dat <- make_dat()

  # Not conditional on data: prob and m are the scalar slots on every path.
  expect_error(declare_ra(prob_unit = 0.5, ra_type = "balanced", data = dat),
               "use `prob`")
  expect_error(declare_ra(N = 20, prob_unit = 0.5, ra_type = "balanced"),
               "use `prob`")
  expect_error(declare_ra(m_unit = 3, data = dat), "use `m`")
  expect_error(declare_ra(N = 20, m_unit = 3), "use `m`")

  # And the argument it points at works, on the balanced path too.
  expect_equal(class(declare_ra(prob = 0.5, ra_type = "balanced", data = dat))[2],
               "ra_balanced")
  expect_equal(class(declare_ra(m = 10, data = dat))[2], "ra_complete")

  # N = 1 is the one case where one value is one value per unit.
  expect_equal(class(declare_ra(N = 1, prob_unit = 0.5, ra_type = "balanced"))[2],
               "ra_balanced")
})

test_that("balanced_ra() itself still recycles, having no prob argument", {
  expect_length(balanced_ra(4), 4)
  expect_length(balanced_ra(prob_unit = 0.5, N = 10), 10)
  expect_false("prob" %in% names(formals(balanced_ra)))
})

test_that("a per-unit argument of the wrong length is refused", {
  dat <- make_dat()
  dat$p <- seq(0.3, 0.7, length.out = nrow(dat))
  expect_error(declare_ra(prob_unit = p[1:5], ra_type = "balanced", data = dat),
               "but `data` has")
})

test_that("permutation_matrix is not resolved against data", {
  dat <- make_dat()
  perm <- replicate(4, sample(rep(0:1, each = nrow(dat) / 2)))
  d <- declare_ra(permutation_matrix = perm, data = dat)
  expect_equal(class(d)[2], "ra_custom")
})
