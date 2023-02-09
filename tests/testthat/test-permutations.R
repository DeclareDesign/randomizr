context("Permutations")


expect_declaration <- function(declaration) {
  perms <- obtain_permutation_matrix(declaration)
  
  n_perms <- obtain_num_permutations(declaration)
  perms_probs <- obtain_permutation_probabilities(declaration)
  
  expect_equal(ncol(perms), n_perms)
  expect_equal(ncol(perms), length(perms_probs))
  expect_equal(sum(perms_probs), 1)
  
  expect_equal(n_perms, ncol(unique(perms, margin = 2)))
  
  draw <- conduct_ra(declaration)
  
  expect_gt(length(draw), 0)
  expect_true(all(draw %in% declaration$conditions))
  
}

test_that("RA N=4 simple=TRUE", {
  declaration <- declare_ra(N = 4, simple = TRUE)
  expect_declaration(declaration)
})


test_that("RA N=4 prob=.01 simple=TRUE", {
  declaration <- declare_ra(N = 4, prob = .01, simple = TRUE)
  expect_declaration(declaration)
})


test_that("RA N = 6, prob_each = c(.5, .5), conditions = c(0,1)", {
  declaration <-
    declare_ra(6, prob_each = c(.5, .5), conditions = c(0, 1))
  expect_declaration(declaration)
})


test_that("testing internal methods N=6 prob_each=.5 condtions=0:1", {
  perms <-
    complete_ra_permutations(6, prob_each = c(.5, .5), conditions = c(0, 1))
  perm_probs <-
    complete_ra_permutation_probabilities(6, prob_each = c(.5, .5),
                                          conditions = c(0, 1))
  n_perms <-
    complete_ra_num_permutations(6, prob_each = c(.5, .5), 
                                 conditions = c(0, 1))
  
  expect_equal(n_perms,
               choose(6, 3))
  
  expect_equal(n_perms,
               ncol(perms))
  
  expect_equal(n_perms,
               length(perm_probs))
  
  expect_equal(1,
               sum(perm_probs))
  
  expect_equal(3,
               unique(colSums(perms)))
})


test_that("testing internal methods N=10", {
  n_perms <-
    complete_ra_num_permutations(10,
                                 prob_each = c(.5, .5),
                                 conditions = c(0, 1))
  perms <-
    complete_ra_permutations(10,
                             prob_each = c(.5, .5),
                             conditions = c(0, 1))
  perm_probs <-
    complete_ra_permutation_probabilities(10,
                                          prob_each = c(.5, .5),
                                          conditions = c(0, 1))
  
  expect_equal(n_perms,
               choose(10, 5))
  
  expect_equal(n_perms,
               ncol(perms))
  
  expect_equal(n_perms,
               length(perm_probs))
  
  expect_equal(1,
               sum(perm_probs))
  
  expect_equal(5,
               unique(colSums(perms)))
  
})


test_that("combinatorics are correct, 6 choose 3", {
  expect_equal(complete_ra_num_permutations(6, prob_each = c(.5, .5), conditions = c(0, 1)),
               choose(6, 3))
  
  perms <-
    complete_ra_permutations(6, prob_each = c(.5, .5), conditions = c(0, 1))
  perm_probs <-
    complete_ra_permutation_probabilities(6, prob_each = c(.5, .5), conditions = c(0, 1))
  
  expect_equal(dim(perms),
               dim(unique(perms)))
  
  
  expect_equal(ncol(perms), length(perm_probs))
  
  expect_equal(sum(perm_probs), 1)
})

test_that("combinatorics are correct, 10 choose 5", {
  expect_equal(complete_ra_num_permutations(
    10,
    prob_each = c(.5, .5),
    conditions = c(0, 1)
  ),
  choose(10, 5))
  
  
  perms <-
    complete_ra_permutations(10,
                             prob_each = c(.5, .5),
                             conditions = c(0, 1))
  perm_probs <-
    complete_ra_permutation_probabilities(10,
                                          prob_each = c(.5, .5),
                                          conditions = c(0, 1))
  
  expect_equal(dim(perms),
               dim(unique(perms)))
  
  
  expect_equal(ncol(perms), length(perm_probs))
  
  expect_equal(sum(perm_probs), 1)
  
})




test_that("combinatorics are correct, 3 / .5", {
  expect_equal(
    complete_ra_num_permutations(3, prob_each = c(.5, .5), conditions = c(0, 1)),
    choose(3, 1) + choose(3, 2)
  )
  
  perms <-
    complete_ra_permutations(3, prob_each = c(.5, .5), conditions = c(0, 1))
  perm_probs <-
    complete_ra_permutation_probabilities(3, prob_each = c(.5, .5), conditions = c(0, 1))
  
  expect_equal(dim(perms),
               dim(unique(perms)))
  
  expect_equal(ncol(perms), length(perm_probs))
  
  expect_equal(sum(perm_probs), 1)
  
  
})

test_that("combinatorics are correct, 10 / .3", {
  expect_equal(
    complete_ra_num_permutations(
      10,
      prob_each = c(1 / 3, 1 / 3, 1 / 3),
      conditions = c("T1", "T2", "T3")
    ),
    choose(10, 3) * choose(10, 4) / 2
  )
  perms <-
    complete_ra_permutations(10,
                             prob_each = c(1 / 3, 1 / 3, 1 / 3),
                             conditions = c("T1", "T2", "T3"))
  
  perm_probs <-
    complete_ra_permutation_probabilities(10,
                                          prob_each = c(1 / 3, 1 / 3, 1 / 3),
                                          conditions = c("T1", "T2", "T3"))
  
  expect_equal(dim(perms),
               dim(unique(perms)))
  
  
  expect_equal(ncol(perms), length(perm_probs))
  
  expect_equal(sum(perm_probs), 1)
  
  
})



test_that("declare_ra(N=4)", {
  declaration <- declare_ra(N = 4)
  perms <- obtain_permutation_matrix(declaration)
  dim(perms)
  expect_equal(obtain_num_permutations(declaration), 6)
  obtain_permutation_probabilities(declaration)
  
})

test_that("blocked, diff size group", {
  blocks <- c("A", "A", "B", "B", "C", "C", "C")
  declaration <- declare_ra(blocks = blocks)
  perms <- obtain_permutation_matrix(declaration)
  dim(perms)
  expect_equal(obtain_num_permutations(declaration), 24)
  obtain_permutation_probabilities(declaration)
})

# blocked, same size group --------------------------------------------------

blocks <- c("A", "A", "B", "B", "C", "C")
declaration <- declare_ra(blocks = blocks)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)
obtain_permutation_probabilities(declaration)

# clustered, diff size clusters --------------------------------------------------

clusters <- c("A", "B", "A", "B", "C", "C", "C")
declaration <- declare_ra(clusters = clusters)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)
obtain_permutation_probabilities(declaration)

# clustered, same size clusters --------------------------------------------------

clusters <- c("A", "B", "A", "B", "C", "C")
declaration <- declare_ra(clusters = clusters)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)
obtain_permutation_probabilities(declaration)


# block clustered, simple ----------------------------------------

clusters <- c("A", "B", "A", "B", "C", "C", "D", "D")
blocks <- rep(1:2, each = 4)
declaration <- declare_ra(clusters = clusters,
                          blocks = blocks)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)
obtain_permutation_probabilities(declaration)


# block clustered, uneven ----------------------------------------

clusters <- c("A", "B", "A", "B", "C", "C", "D", "D", "D", "E", "E")
blocks <- rep(1:2, times = c(4, 7))
declaration <- declare_ra(clusters = clusters,
                          blocks = blocks)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)
obtain_permutation_probabilities(declaration)

# test against RI package

y <- c(8, 6, 2, 0, 3, 1, 1, 1, 2, 2, 0, 1, 0, 2, 2, 4, 1, 1)
Z <- c(1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0)
cluster <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)
block <- c(rep(1, 4), rep(2, 6), rep(3, 8))

df <- data.frame(Z, cluster, block)

declaration <- declare_ra(
  blocks = df$block,
  clusters = df$cluster,
  block_m = tapply(df$Z, df$block, sum) / 2
)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)

mine <- obtain_permutation_matrix(declaration = declaration)
rowMeans(mine) - declaration$probabilities_matrix[, 2]

test_that("testing internal methods N=3", {
  n_perms <-
    complete_ra_num_permutations(3, prob_each = c(.5, .5), conditions = c(0, 1))
  perms <-
    complete_ra_permutations(3, prob_each = c(.5, .5), conditions = c(0, 1))
  perm_probs <-
    complete_ra_permutation_probabilities(3, prob_each = c(.5, .5), conditions = c(0, 1))
  
  expect_equal(n_perms,
               choose(3, 1) + choose(3, 2))
  
  expect_equal(n_perms,
               ncol(perms))
  
  expect_equal(n_perms,
               length(perm_probs))
  
  expect_equal(1,
               sum(perm_probs))
})


test_that("testing internal methods N=10 p=1/3", {
  n_perms <-
    complete_ra_num_permutations(10,
                                 prob_each = c(1 / 3, 1 / 3, 1 / 3),
                                 conditions = c("T1", "T2", "T3"))
  perms <-
    complete_ra_permutations(10,
                             prob_each = c(1 / 3, 1 / 3, 1 / 3),
                             conditions = c("T1", "T2", "T3"))
  perm_probs <-
    complete_ra_permutation_probabilities(10,
                                          prob_each = c(1 / 3, 1 / 3, 1 / 3),
                                          conditions = c("T1", "T2", "T3"))
  
  expect_equal(n_perms,
               choose(10, 3) * choose(7, 4) * 3)
  
  expect_equal(n_perms,
               ncol(perms))
  
  expect_equal(n_perms,
               ncol(unique(perms, margin = 2)))
  
  expect_equal(n_perms,
               length(perm_probs))
  
  expect_equal(1,
               sum(perm_probs))
  
})


test_that("N=4 complete", {
  declaration <- declare_ra(N = 4)
  expect_declaration(declaration)
})


test_that("blocked, diff size group", {
  blocks <- c("A", "A", "B", "B", "C", "C", "C")
  declaration <- declare_ra(blocks = blocks)
  expect_declaration(declaration)
})


test_that("blocked, same size group", {
  blocks <- c("A", "A", "B", "B", "C", "C")
  declaration <- declare_ra(blocks = blocks)
  expect_declaration(declaration)
  
})


test_that("clustered, diff size clusters", {
  clusters <- c("A", "B", "A", "B", "C", "C", "C")
  declaration <- declare_ra(clusters = clusters)
  expect_declaration(declaration)
})


test_that("clustered, same size clusters", {
  clusters <- c("A", "B", "A", "B", "C", "C")
  declaration <- declare_ra(clusters = clusters)
  expect_declaration(declaration)
})


test_that("block clustered, simple", {
  clusters <- c("A", "B", "A", "B", "C", "C", "D", "D")
  blocks <- rep(1:2, each = 4)
  declaration <- declare_ra(clusters = clusters, blocks = blocks)
  expect_declaration(declaration)
})


test_that("block clustered, uneven", {
  clusters <- c("A", "B", "A", "B", "C", "C", "D", "D", "D", "E", "E")
  blocks <- rep(1:2, times = c(4, 7))
  declaration <- declare_ra(clusters = clusters, blocks = blocks)
  expect_declaration(declaration)
})


test_that("matches ri package", {
  skip_if_not_installed("ri")
  
  # test against RI package
  
  y <- c(8, 6, 2, 0, 3, 1, 1, 1, 2, 2, 0, 1, 0, 2, 2, 4, 1, 1)
  Z <- c(1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0)
  cluster <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)
  block <- c(rep(1, 4), rep(2, 6), rep(3, 8))
  
  df <- data.frame(Z, cluster, block)
  
  declaration <- declare_ra(
    blocks = df$block,
    clusters = df$cluster,
    block_m = tapply(df$Z, df$block, sum) / 2
  )
  expect_declaration(declaration)
  
  mine <- obtain_permutation_matrix(declaration = declaration)
  rowMeans(mine) - declaration$probabilities_matrix[, 2]
  
  # theirs <- ri::genperms(df$Z, df$block, df$cluster)
  
  expect_equal(anyDuplicated(mine, MARGIN = 2),   0)
  # expect_equal(anyDuplicated(theirs, MARGIN = 2), 0)
  # expect_identical(sort(apply(
  #   mine,  2, paste, sep = "", collapse = ""
  # )),
  # sort(apply(
  #   theirs, 2, paste, sep = "", collapse = ""
  # )))
})

# test big numbers


test_that("big numbers", {
  declaration <- declare_ra(20)
  perms <- obtain_permutation_matrix(declaration)
  n_perms <- obtain_num_permutations(declaration)
  
  expect_equal(n_perms, choose(20, 10))
  expect_equal(dim(perms), c(20, 10000))
})


test_that("test wacky declarations", {
  declaration <- declare_ra(N = 5, prob_each = c(.49, .51))
  expect_declaration(declaration)
  
  perms <- obtain_permutation_matrix(declaration)
  perm_probs <- obtain_permutation_probabilities(declaration)
  
  expect_identical(declaration$probabilities_matrix[, 2],
                   rep(.51, 5))
  
  # correctly WRONG because the perms have different probs!
  expect_equal(rowMeans(perms),
               rep(.5, 5))
  
  # correctly correct!
  expect_equal(drop(perms %*% perm_probs),
               rep(.51, 5))
})


test_that("Overflowing N", {
  declaration <- declare_ra(N = 958, m = 479)
  
  expect_equal(obtain_num_permutations(declaration), Inf)
  expect_equal(dim(obtain_permutation_matrix(declaration)), c(958, 10000))
  
})


test_that("multinomial coefficient helper", {
  expect_error(multinomial_coefficient(200, 3:100))
})


test_that("kyle's declation", {
  declaration <- declare_ra(N = 8, m_each = c(2, 2, 2, 2))
  perms <- obtain_permutation_matrix(declaration)
  expect_equal(2, unique(apply(
    perms,
    2,
    FUN = function(x)
      sum(x == "T1")
  )))
  
})
