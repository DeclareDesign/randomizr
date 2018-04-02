
context("Cluster Random Sampling")
clusters <- rep(letters[1:15], times = 1:15)

#debugonce(cluster_rs_probabilities)
test_that("Two Group Designs", {
  S <- cluster_rs(clusters = clusters)
  probs <- cluster_rs_probabilities(clusters = clusters)
  expect_true(all(colSums(table(S, clusters) != 0) == 1))
  expect_true(all(probs == .5))
})

test_that("Two Gropu n = 13", {
  S <- cluster_rs(clusters = clusters, n = 13)
  probs <- cluster_rs_probabilities(clusters = clusters, n = 13)
  expect_true(all(colSums(table(S, clusters) != 0) == 1))
  expect_true(all(probs == 13/15))
})

test_that("prob = .5", {
  S <- cluster_rs(clusters = clusters, prob = .5)
  probs <- cluster_rs_probabilities(clusters = clusters, prob = .5)
  expect_true(all(colSums(table(S, clusters) != 0) == 1))
  expect_true(all(probs == .5))
})

test_that("prob = 0", {
  
S <- cluster_rs(clusters = clusters, prob = 0)
probs <- cluster_rs_probabilities(clusters = clusters, prob = 0)
expect_true(all(S== 0))
expect_true(all(probs== 0))
})

test_that("prob = 1", {
  
  S <- cluster_rs(clusters = clusters, prob = 1)
  probs <- cluster_rs_probabilities(clusters = clusters, prob = 1)
  expect_true(all(S== 1))
  expect_true(all(probs== 1))
})

test_that("clust simple ", {
  clusters = gl(10,2)
  S <- cluster_rs(clusters = clusters, simple = TRUE)
  s_prob <- cluster_rs_probabilities(clusters = clusters, simple = TRUE)
  expect_true(all(rowSums(table(clusters, S) > 0 ) == 1))
  expect_true(all(s_prob > 0 & s_prob < 1))
})

test_that("n and simple is error", {
  expect_error(cluster_rs(clusters = gl(10,2), simple = TRUE, n=3))
  expect_error(cluster_rs_probabilities(clusters = gl(10,2), simple = TRUE, n=3))
})