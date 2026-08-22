library(randomizr)

# check_inputs = FALSE is meant to skip validation in a simulation loop. It used
# to skip the derivation of num_arms and conditions along with it, which the
# assignment functions need and cannot recover elsewhere, so every blocked and
# clustered call errored. These pin down that it skips only the checking.

designs <- function() {
  bl <- rep(1:4, each = 15)
  cl <- rep(1:12, each = 5)
  clb <- rep(1:4, each = 15)
  list(
    simple = quote(simple_ra(N = 60)),
    simple_prob = quote(simple_ra(N = 60, prob = 0.3)),
    simple_each = quote(simple_ra(N = 60, prob_each = c(.2, .3, .5))),
    complete = quote(complete_ra(N = 60)),
    complete_m = quote(complete_ra(N = 60, m = 17)),
    complete_m_each = quote(complete_ra(N = 60, m_each = c(10, 20, 30))),
    complete_arms = quote(complete_ra(N = 60, num_arms = 4)),
    block = quote(block_ra(blocks = bl)),
    block_prob = quote(block_ra(blocks = bl, prob = 0.4)),
    block_arms = quote(block_ra(blocks = bl, num_arms = 3)),
    block_m = quote(block_ra(blocks = bl, block_m = c(3, 4, 5, 6))),
    block_each = quote(block_ra(blocks = bl, prob_each = c(.2, .3, .5))),
    block_conditions = quote(block_ra(blocks = bl, conditions = c("a", "b"))),
    cluster = quote(cluster_ra(clusters = cl)),
    cluster_m = quote(cluster_ra(clusters = cl, m = 5)),
    cluster_arms = quote(cluster_ra(clusters = cl, num_arms = 3)),
    cluster_each = quote(cluster_ra(clusters = cl, prob_each = c(.2, .3, .5))),
    bandc = quote(block_and_cluster_ra(blocks = clb, clusters = cl)),
    bandc_arms = quote(block_and_cluster_ra(blocks = clb, clusters = cl, num_arms = 3)),
    bandc_each = quote(block_and_cluster_ra(blocks = clb, clusters = cl, prob_each = c(.4, .6)))
  )
}

test_that("check_inputs = FALSE assigns rather than erroring", {
  bl <- rep(1:4, each = 15)
  cl <- rep(1:12, each = 5)
  clb <- rep(1:4, each = 15)
  for (nm in names(designs())) {
    call_false <- as.call(c(as.list(designs()[[nm]]), check_inputs = FALSE))
    expect_silent(z <- eval(call_false))
    expect_length(z, 60)
    expect_false(anyNA(z))
  }
})

test_that("check_inputs = FALSE draws exactly what check_inputs = TRUE draws", {
  bl <- rep(1:4, each = 15)
  cl <- rep(1:12, each = 5)
  clb <- rep(1:4, each = 15)
  for (nm in names(designs())) {
    d <- designs()[[nm]]
    set.seed(20260823)
    with_check <- replicate(50, as.character(eval(as.call(c(as.list(d), check_inputs = TRUE)))))
    set.seed(20260823)
    without <- replicate(50, as.character(eval(as.call(c(as.list(d), check_inputs = FALSE)))))
    expect_identical(without, with_check, info = nm)
  }
})

test_that("the derived num_arms and conditions match on both paths", {
  bl <- rep(1:4, each = 15)
  cl <- rep(1:12, each = 5)
  clb <- rep(1:4, each = 15)
  pairs <- list(
    quote(simple_ra_probabilities(N = 60)),
    quote(complete_ra_probabilities(N = 60)),
    quote(complete_ra_probabilities(N = 60, prob_each = c(.2, .3, .5))),
    quote(block_ra_probabilities(blocks = bl)),
    quote(block_ra_probabilities(blocks = bl, num_arms = 3)),
    quote(cluster_ra_probabilities(clusters = cl)),
    quote(block_and_cluster_ra_probabilities(blocks = clb, clusters = cl))
  )
  for (d in pairs) {
    expect_identical(
      eval(as.call(c(as.list(d), check_inputs = FALSE))),
      eval(as.call(c(as.list(d), check_inputs = TRUE)))
    )
  }
})

test_that("an impossible design is refused whether or not the checks run", {
  # check_inputs = FALSE waives the checking of a design, not memory safety.
  # block_assign_cpp() fills its buffer by counting down from the block size, so
  # an out-of-range block_m used to start the loop at a negative index, write off
  # the front of the heap, and bring R down with a bus error a few calls later.
  expect_error(complete_ra(N = 10, m = 20, check_inputs = TRUE))
  expect_error(complete_ra(N = 10, m = 20, check_inputs = FALSE))

  blocks <- rep(1:2, each = 5)
  expect_error(block_ra(blocks = blocks, block_m = c(99, 99), check_inputs = TRUE))
  expect_error(block_ra(blocks = blocks, block_m = c(99, 99), check_inputs = FALSE),
               "5 units but 99")
  expect_error(block_ra(blocks = blocks, block_m = c(-1, 2), check_inputs = FALSE),
               "-1 were requested")
  expect_error(block_ra(blocks = blocks, block_prob = c(1.5, 0.5), check_inputs = FALSE),
               "not in \\[0, 1\\]")

  # Repeatedly, because the failure it replaces was cumulative heap corruption
  # that survived the first call and killed the second.
  for (i in 1:50) {
    expect_error(block_ra(blocks = blocks, block_m = c(99, 99), num_arms = 2L,
                          conditions = 0:1, check_inputs = FALSE))
  }
})

test_that("the bounds guard leaves legal assignments untouched", {
  blocks <- rep(1:6, each = 20)
  set.seed(4242)
  a <- replicate(50, paste(block_ra(blocks = blocks, block_m = c(3,4,5,6,7,8)), collapse = ""))
  set.seed(4242)
  b <- replicate(50, paste(block_ra(blocks = blocks, block_m = c(3,4,5,6,7,8),
                                    check_inputs = FALSE), collapse = ""))
  expect_identical(a, b)
})
