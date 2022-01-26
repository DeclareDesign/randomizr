

context("Blocked and Clustered Random Assignments")

clusters <- rep(letters, times = 1:26)
blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:5]] <- "block_1"
blocks[clusters %in% letters[6:10]] <- "block_2"
blocks[clusters %in% letters[11:15]] <- "block_3"
blocks[clusters %in% letters[16:20]] <- "block_4"
blocks[clusters %in% letters[21:26]] <- "block_5"



expect_only_one_per_cluster <- function(Z, w = 1, clust = clusters) {
  expect_true(all(colSums(table(Z, clust) != 0) == w))
}

test_that("defaults", {
  df <- data.frame(blocks, clusters)
  df <- df[sample(seq_len(nrow(df)),nrow(df)), ]
  
  Z <-
    block_and_cluster_ra(clusters = df$clusters, blocks = df$blocks)
  
  expect_only_one_per_cluster(Z, clust = df$clusters)
  
})


test_that("num_arms = 3", {
  Z <-
    block_and_cluster_ra(clusters = clusters,
                         blocks = blocks,
                         num_arms = 3)
  expect_only_one_per_cluster(Z)
})

test_that("prob_each three arm", {
  Z <-
    block_and_cluster_ra(
      clusters = clusters,
      blocks = blocks,
      prob_each = c(.2, .5, .3)
    )
  expect_only_one_per_cluster(Z)
})

test_that("prob_each", {
  Z <-
    block_and_cluster_ra(clusters = clusters,
                         blocks = blocks,
                         prob_each = c(.2, .8))
  expect_only_one_per_cluster(Z)
})

test_that("num_arms=2", {
  Z <-
    block_and_cluster_ra(clusters = clusters,
                         blocks = blocks,
                         num_arms = 2)
  expect_only_one_per_cluster(Z)
})

test_that("m=2", {
  Z <-
    block_and_cluster_ra(clusters = clusters,
                         blocks = blocks,
                         m = 2)
  expect_only_one_per_cluster(Z)
})

test_that("block_m_each", {
  block_m_each <- rbind(c(2, 3),
                        c(1, 4),
                        c(3, 2),
                        c(2, 3),
                        c(5, 1))
  
  Z <-
    block_and_cluster_ra(clusters = clusters,
                         blocks = blocks,
                         block_m_each = block_m_each)
  expect_only_one_per_cluster(Z)
  
})

test_that("prob = .5", {
  Z <-
    block_and_cluster_ra(clusters = clusters,
                         blocks = blocks,
                         prob = .5)
  expect_only_one_per_cluster(Z)
  
})

test_that("prob = 0", {
  Z <-
    block_and_cluster_ra(clusters = clusters,
                         blocks = blocks,
                         prob = 0)
  expect_true(all(Z == 0))
})

test_that("prob = 1", {
  Z <-
    block_and_cluster_ra(clusters = clusters,
                         blocks = blocks,
                         prob = 1)
  expect_true(all(Z == 1))
})
