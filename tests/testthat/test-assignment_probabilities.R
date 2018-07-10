context("Probabilities: Complete")

expect_constCol <- function(matrix, val, N = 100) {
  test <-
    sweep(
      matrix,
      MARGIN = 2,
      STATS = val,
      FUN = function(x, y)
        abs(x - y) < 1e-7
    )
  expect_true(all(test))
  expect_equal(nrow(matrix), N)
  if (length(val) > 1) {
    expect_equal(ncol(matrix), length(val))
  }
  if (is.character(names(val))) {
    expect_equal(paste0("prob_", names(val)), colnames(matrix))
  }
  
}

test_that("2-arm designs", {
  expect_constCol(complete_ra_probabilities(N = 100), .5)
  expect_constCol(complete_ra_probabilities(N = 100, m = 50), .5)
  expect_constCol(complete_ra_probabilities(N = 100, prob = .3), c(.7, .3))
  
  expect_constCol(
    complete_ra_probabilities(
      N = 100,
      m_each = c(30, 70),
      conditions = c("control", "treatment")
    ),
    c(control = .3, treatment = .7)
  )
})

test_that("Multi-arm Designs", {
  expect_constCol(complete_ra_probabilities(N = 100, num_arms = 3), 1 / 3)
  expect_constCol(complete_ra_probabilities(N = 100,
                                            m_each = c(30, 30, 40)),
                  c(.3, .3, .4))
  
  expect_constCol(
    complete_ra_probabilities(
      N = 100,
      m_each = c(30, 30, 40),
      conditions = c("control", "placebo", "treatment")
    ),
    c(
      control = .3,
      placebo = .3,
      treatment = .4
    )
  )
  
  expect_constCol(
    complete_ra_probabilities(
      N = 100,
      conditions = c("control", "placebo", "treatment")
    ),
    c(
      control = 1,
      placebo = 1,
      treatment = 1
    ) / 3
  )
  
  expect_constCol(complete_ra_probabilities(N = 100, prob_each = c(.2, .7, .1)),
                  c(.2, .7, .1))
  
  expect_constCol(complete_ra_probabilities(N = 101, prob_each = c(.2, .7, .1)),
                  c(.2, .7, .1),
                  101)
})


test_that("Special cases", {
  expect_constCol(complete_ra_probabilities(
    2,
    m_each = c(1, 0, 1),
    conditions = c("T1", "T2", "T3")
  ),
  c(T1 = .5, T2 = 0, T3 = .5),
  2)
  
  expect_constCol(complete_ra_probabilities(
    2,
    m_each = c(0, 0, 2),
    conditions = c("T1", "T2", "T3")
  ),
  c(T1 = 0, T2 = 0, T3 = 1),
  2)
})


# Simple Designs ----------------------------------------------------------

context("Probabilities: Simple")

test_that("Two Group Designs", {
  expect_constCol(simple_ra_probabilities(N = 100),
                  c(.5, .5))
  expect_constCol(simple_ra_probabilities(N = 100, prob = 0.5),
                  c(.5, .5))
  expect_constCol(
    simple_ra_probabilities(
      N = 100,
      prob_each = c(0.3, 0.7),
      conditions = c("control", "treatment")
    ),
    c(control = .3, treatment = .7)
  )
})

test_that("Multi-arm Designs", {
  expect_constCol(simple_ra_probabilities(N = 100, num_arms = 3),
                  1 / 3)
  
  expect_constCol(simple_ra_probabilities(N = 100, prob_each = c(0.3, 0.3, 0.4)),
                  c(.3, .3, .4))
  
  expect_constCol(
    simple_ra_probabilities(
      N = 100,
      prob_each = c(0.3, 0.3, 0.4),
      conditions = c("control", "placebo", "treatment")
    ),
    c(
      control = 0.3,
      placebo = 0.3,
      treatment = 0.4
    )
  )
  
  expect_constCol(
    simple_ra_probabilities(
      N = 100,
      conditions = c("control", "placebo", "treatment")
    ),
    c(
      control = 1,
      placebo = 1,
      treatment = 1
    ) / 3
  )
  
})
# Blocked Designs ---------------------------------------------------------

context("Probabilities: Blocked")
blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))

test_that("Blocked 50/100/200", {
  expect_constCol(block_ra_probabilities(blocks = blocks),
                  .5,
                  350)
  
})

test_that("Blocked 50/100/200 block_m_each even", {
  block_m_each <- rbind(c(25, 25),
                        c(50, 50),
                        c(100, 100))
  
  expect_constCol(block_ra_probabilities(blocks = blocks, block_m_each = block_m_each),
                  .5,
                  350)
  
})

test_that("Blocked 50/100/200 block_m_each different", {
  # Different weights per block
  block_m_each <- rbind(c(10, 40),
                        c(30, 70),
                        c(50, 150))
  
  mapply(
    expect_constCol,
    by(
      block_ra_probabilities(
        blocks = blocks,
        block_m_each = block_m_each,
        conditions = c("control", "treatment")
      ),
      blocks,
      as.matrix
    ),
    list(
      c(control = .2, treatment = .8),
      c(control = .3, treatment = .7),
      c(control = .25, treatment = .75)
    ),
    list(50, 100, 200)
  )
})

test_that("Blocked 50/100/200 3 arms", {
  expect_constCol(block_ra_probabilities(blocks = blocks, num_arms = 3),
                  1 / 3,
                  350)
})

test_that("Blocked 50/100/200 3 arms block_m_each", {
  block_m_each <- rbind(c(10, 20, 20),
                        c(30, 50, 20),
                        c(50, 75, 75))
  
  mapply(
    expect_constCol,
    by(
      block_ra_probabilities(blocks = blocks, block_m_each = block_m_each),
      blocks,
      as.matrix
    ),
    list(c(.2, .4, .4), c(.3, .5, .2), c(.25, .375, .375)),
    as.list(rowSums(block_m_each))
  )
  
  
  rownames(block_m_each) <- c("control", "placebo", "treatment")
  
  mapply(
    expect_constCol,
    by(
      block_ra_probabilities(
        blocks = blocks,
        block_m_each = block_m_each,
        conditions = c("control", "placebo", "treatment")
      ),
      blocks,
      as.matrix
    ),
    list(
      c(
        control = .2,
        placebo = .4,
        treatment = .4
      ),
      c(
        control = .3,
        placebo = .5,
        treatment = .2
      ),
      c(
        control = .25,
        placebo = .375,
        treatment = .375
      )
    ),
    as.list(rowSums(block_m_each))
  )
})

test_that("Blocked 50/100/200 3 arms prob_each", {
  expect_constCol(block_ra_probabilities(blocks = blocks, prob_each = c(.1, .1, .8)),
                  c(.1, .1, .8),
                  350)
})

test_that("Blocked 50/100/200 3 arms prob_each matrix", {
  block_prob_each <- rbind(c(.1, .2, .7),
                           c(.1, .3, .6),
                           c(1 / 3, 1 / 3, 1 / 3))
  
  mapply(
    expect_constCol,
    by(
      block_ra_probabilities(blocks = blocks, block_prob_each = block_prob_each),
      blocks,
      as.matrix
    ),
    split(block_prob_each, 1:3),
    as.list(table(blocks))
  )
})

context("Probabilities: Clustered")
clusters <- rep(letters[1:15], times = 1:15)

test_that("Clustered Two Arm", {
  expect_constCol(cluster_ra_probabilities(clusters = clusters),
                  .5,
                  length(clusters))
  
  expect_constCol(cluster_ra_probabilities(clusters = clusters, m = 10),
                  c(1, 2) / 3,
                  length(clusters))
  
  expect_constCol(
    cluster_ra_probabilities(
      clusters = clusters,
      m_each = c(9, 6),
      conditions = c("control", "treatment")
    ),
    c(control = .6, treatment = .4),
    length(clusters)
  )
  
})

# Multi-arm Designs
test_that("Clustered Three Arm", {
  expect_constCol(cluster_ra_probabilities(clusters = clusters, num_arms = 3),
                  1 / 3,
                  length(clusters))
  
  expect_constCol(
    cluster_ra_probabilities(clusters = clusters, m_each = c(3, 3, 9)),
    c(3, 3, 9) / 15,
    length(clusters)
  )
  
  expect_constCol(
    cluster_ra_probabilities(
      clusters = clusters,
      m_each = c(3, 3, 9),
      conditions = c("control", "placebo", "treatment")
    ),
    c(
      control = 3,
      placebo = 3,
      treatment = 9
    ) / 15,
    length(clusters)
  )
  
  expect_constCol(
    cluster_ra_probabilities(
      clusters = clusters,
      conditions = c("control", "placebo", "treatment")
    ),
    c(
      control = 1,
      placebo = 1,
      treatment = 1
    ) / 3,
    length(clusters)
  )
  
  expect_constCol(
    cluster_ra_probabilities(clusters = clusters, prob_each = c(.1, .2, .7)),
    c(.1, .2, .7),
    length(clusters)
  )
})

context("Probabilities: Blocked and Clustered")
# Blocked and clustered designs -------------------------------------------
clusters <- rep(letters[1:15], times = 1:15)
blocks <-
  paste0("block_", 1:5)[cut(match(clusters, letters), 0:5 * 3, labels = FALSE)]

test_that("block, cluster", {
  expect_constCol(
    block_and_cluster_ra_probabilities(clusters = clusters, blocks = blocks),
    .5,
    length(clusters)
  )
  
  expect_constCol(
    block_and_cluster_ra_probabilities(
      clusters = clusters,
      blocks = blocks,
      num_arms = 3
    ),
    c(1, 1, 1) / 3,
    length(clusters)
  )
  
  expect_constCol(
    block_and_cluster_ra_probabilities(
      clusters = clusters,
      blocks = blocks,
      prob_each = c(.2, .5, .3)
    ),
    c(.2, .5, .3),
    length(clusters)
  )
  
  
  block_m_each <- rbind(c(1, 2),
                        c(2, 1),
                        c(1, 2),
                        c(2, 1),
                        c(2, 1))
  
  mapply(
    expect_constCol,
    by(
      block_and_cluster_ra_probabilities(
        clusters = clusters,
        blocks = blocks,
        block_m_each = block_m_each
      ),
      blocks,
      as.matrix
    ),
    list(c(1, 2) / 3, c(2, 1) / 3, c(1, 2) / 3, c(2, 1) / 3, c(2, 1) / 3),
    as.list(table(blocks))
  )
  
})
