context("Stratified and Clustered Random Sampling")

clusters <- rep(letters, times = 1:26)
strata <- rep(NA, length(clusters))
strata[clusters %in% letters[1:5]] <- "stratum_1"
strata[clusters %in% letters[6:10]] <- "stratum_2"
strata[clusters %in% letters[11:15]] <- "stratum_3"
strata[clusters %in% letters[16:20]] <- "stratum_4"
strata[clusters %in% letters[21:26]] <- "stratum_5"



expect_only_one_per_cluster <- function(Z, w = 1) {
  expect_true(all(colSums(table(Z, clusters) != 0) == w))
}


test_that("defaults", {
  S <- strata_and_cluster_rs(clusters = clusters, strata = strata)
  probs <-
    strata_and_cluster_rs_probabilities(clusters = clusters, strata = strata)
  
  expect_only_one_per_cluster(S)
  
})

test_that("n=1", {
  S <-
    strata_and_cluster_rs(clusters = clusters,
                          strata = strata,
                          n = 2)
  probs <-
    strata_and_cluster_rs_probabilities(clusters = clusters,
                                        strata = strata,
                                        n = 2)
  
  expect_only_one_per_cluster(S)
  
})

test_that("prob=.5", {
  S <- strata_and_cluster_rs(clusters = clusters,
                             strata = strata,
                             prob = .5)
  probs <-
    strata_and_cluster_rs_probabilities(clusters = clusters, strata = strata)
  
  expect_only_one_per_cluster(S)
})

test_that("prob = 0", {
  S <- strata_and_cluster_rs(clusters = clusters,
                             strata = strata,
                             prob = 0)
  probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
                                               strata = strata,
                                               prob = 0)
  
  expect_true(all(S == 0))
  expect_true(all(probs == 0))
})

test_that("prob = 1", {
  S <- strata_and_cluster_rs(clusters = clusters,
                             strata = strata,
                             prob = 1)
  probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
                                               strata = strata,
                                               prob = 1)
  
  expect_true(all(S == 1))
  expect_true(all(probs == 1))
})

test_that("strata_n", {
  S <- strata_and_cluster_rs(
    clusters = clusters,
    strata = strata,
    strata_n = c(2, 3, 2, 3, 2)
  )
  
  probs <- strata_and_cluster_rs_probabilities(
    clusters = clusters,
    strata = strata,
    strata_n = c(2, 3, 2, 3, 2)
  )
  
  expect_only_one_per_cluster(S)
})

test_that("strata_prob", {
  S <- strata_and_cluster_rs(
    clusters = clusters,
    strata = strata,
    strata_prob = c(.1, .2, .3, .4, .5)
  )
  
  probs <- strata_and_cluster_rs_probabilities(
    clusters = clusters,
    strata = strata,
    strata_prob = c(.1, .2, .3, .4, .5)
  )
  
  expect_only_one_per_cluster(S)
})
