context("Declarations: Balanced Random Assignments")

test_that("declare_ra(N, prob = 0.5) remains complete, not cube", {
  d <- declare_ra(N = 100, prob = 0.5)
  expect_equal(class(d)[2], "ra_complete")
  expect_equal(conduct_ra(declare_ra(N = 1, prob = 1)), 1)
})

test_that("varying prob_unit without ra_type still errors as complete", {
  expect_error(
    declare_ra(N = 4, prob_unit = c(0.2, 0.4, 0.6, 0.8)),
    "must be the same for all units"
  )
})

test_that("two-arm declare → conduct → obtain round-trip", {
  p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
  d <- declare_ra(prob_unit = p, ra_type = "balanced")
  expect_equal(class(d)[2], "ra_balanced")
  expect_equal(d$N, 6)
  expect_equal(d$probabilities_matrix[, "prob_1"], p)

  set.seed(11)
  Z <- conduct_ra(d)
  expect_length(Z, 6)
  expect_true(all(Z %in% 0:1))
  expect_equal(sum(Z), 3)

  pr <- obtain_condition_probabilities(d, Z)
  expect_equal(pr, ifelse(Z == 1, p, 1 - p))

  expect_output(print(d), "Balanced random assignment")
  expect_equal(obtain_num_permutations(d), Inf)
})

test_that("prob_unit_each selects balanced without ra_type", {
  P <- cbind(c(0.15, 0.47), c(0.65, 0.48), c(0.20, 0.05))
  d <- declare_ra(prob_unit_each = P)
  expect_equal(class(d)[2], "ra_balanced")
  expect_equal(unname(d$probabilities_matrix), unname(P), tolerance = 1e-12)

  set.seed(12)
  Z <- conduct_ra(d)
  expect_length(Z, 2)
  expect_true(all(Z %in% c("T1", "T2", "T3")))
  pr <- obtain_condition_probabilities(d, Z)
  expect_equal(pr, P[cbind(1:2, match(as.character(Z), c("T1", "T2", "T3")))])
})

test_that("blocked and clustered balanced declarations", {
  districts <- rep(c("north", "south"), each = 3)
  d_b <- declare_ra(blocks = districts, ra_type = "balanced")
  expect_equal(class(d_b)[2], "ra_balanced")
  set.seed(13)
  Zb <- conduct_ra(d_b)
  expect_equal(sum(Zb), 3)
  north <- sum(Zb[districts == "north"])
  expect_true(north %in% 1:2)
  expect_equal(
    obtain_condition_probabilities(d_b, Zb),
    ifelse(Zb == 1, 0.5, 0.5)
  )

  clusters <- rep(1:6, times = c(3, 1, 4, 2, 5, 3))
  p_cluster <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
  p <- p_cluster[clusters]
  d_c <- declare_ra(prob_unit = p, clusters = clusters, ra_type = "balanced")
  expect_equal(class(d_c)[2], "ra_balanced")
  set.seed(14)
  Zc <- conduct_ra(d_c)
  expect_true(all(tapply(Zc, clusters, function(v) length(unique(v)) == 1)))
  expect_equal(obtain_condition_probabilities(d_c, Zc), ifelse(Zc == 1, p, 1 - p))
})

test_that("inline conduct_ra and obtain_condition_probabilities", {
  p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
  set.seed(15)
  Z <- conduct_ra(prob_unit = p, ra_type = "balanced")
  expect_equal(sum(Z), 3)
  pr <- obtain_condition_probabilities(assignment = Z, prob_unit = p,
                                       ra_type = "balanced")
  expect_equal(pr, ifelse(Z == 1, p, 1 - p))
})

test_that("prob and prob_each map onto balanced", {
  d1 <- declare_ra(N = 10, prob = 0.4, ra_type = "balanced")
  expect_equal(class(d1)[2], "ra_balanced")
  expect_equal(d1$probabilities_matrix[, "prob_1"], rep(0.4, 10))

  d2 <- declare_ra(N = 9, prob_each = c(0.2, 0.3, 0.5), ra_type = "balanced")
  expect_equal(class(d2)[2], "ra_balanced")
  expect_equal(d2$probabilities_matrix[1, ], c(prob_T1 = 0.2, prob_T2 = 0.3, prob_T3 = 0.5))
  set.seed(16)
  Z <- conduct_ra(d2)
  expect_true(all(Z %in% c("T1", "T2", "T3")))
})

test_that("balanced refuses count arguments and bad ra_type", {
  expect_error(declare_ra(N = 10, m = 4, ra_type = "balanced"), "not with")
  expect_error(declare_ra(N = 10, simple = TRUE, ra_type = "balanced"),
               "simple = TRUE")
  expect_error(declare_ra(N = 10, ra_type = "complete"),
               "only \"balanced\"")
  expect_error(
    obtain_permutation_probabilities(declare_ra(N = 6, ra_type = "balanced")),
    "does not enumerate cube assignments"
  )
})

test_that("obtain_permutation_matrix samples a balanced declaration", {
  d <- declare_ra(N = 6, ra_type = "balanced")
  pm <- obtain_permutation_matrix(d, maximum_permutations = 20)
  expect_equal(dim(pm), c(6L, 20L))
  expect_true(all(colSums(pm) == 3))
})

test_that("declare_ra(N, ra_type = 'balanced') is balanced at p = 0.5", {
  d <- declare_ra(N = 4, ra_type = "balanced")
  expect_equal(class(d)[2], "ra_balanced")
  expect_equal(d$probabilities_matrix[, "prob_1"], rep(0.5, 4))
  set.seed(31)
  Z <- conduct_ra(d)
  expect_equal(sum(Z), 2)
  expect_true(all(Z %in% 0:1))
})

test_that("conduct_ra matches balanced_ra under the same seed", {
  p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
  d <- declare_ra(prob_unit = p, ra_type = "balanced")
  set.seed(21)
  z_decl <- conduct_ra(d)
  set.seed(21)
  z_direct <- balanced_ra(prob_unit = p)
  expect_equal(z_decl, z_direct)

  P <- cbind(c(0.15, 0.47), c(0.65, 0.48), c(0.20, 0.05))
  d3 <- declare_ra(prob_unit_each = P)
  set.seed(22)
  z3_decl <- conduct_ra(d3)
  set.seed(22)
  z3_direct <- balanced_ra(prob_unit_each = P)
  expect_equal(z3_decl, z3_direct)
})

test_that("obtain_condition_probabilities matches balanced_ra_probabilities", {
  p <- c(0.2, 0.4, 0.6, 0.8)
  d <- declare_ra(prob_unit = p, ra_type = "balanced")
  M <- balanced_ra_probabilities(prob_unit = p)
  expect_equal(d$probabilities_matrix, M)
  Z <- c(0, 1, 1, 0)
  expect_equal(
    obtain_condition_probabilities(d, Z),
    M[cbind(seq_along(Z), match(Z, c(0, 1)))]
  )

  P <- cbind(c(0.15, 0.47), c(0.65, 0.48), c(0.20, 0.05))
  d3 <- declare_ra(prob_unit_each = P)
  M3 <- balanced_ra_probabilities(prob_unit_each = P)
  expect_equal(unname(d3$probabilities_matrix), unname(M3))
  Z3 <- c("T1", "T3")
  expect_equal(
    obtain_condition_probabilities(d3, Z3),
    M3[cbind(1:2, match(Z3, c("T1", "T2", "T3")))]
  )
})

test_that("num_arms and conditions on a balanced declaration", {
  d <- declare_ra(N = 6, num_arms = 2, ra_type = "balanced")
  expect_equal(class(d)[2], "ra_balanced")
  expect_equal(d$num_arms, 2)
  # complete_ra's convention: an explicit num_arms asks for named arms
  expect_equal(d$conditions, c("T1", "T2"))

  d_named <- declare_ra(N = 6, conditions = c("ctrl", "trt"), ra_type = "balanced")
  expect_equal(class(d_named)[2], "ra_balanced")
  set.seed(23)
  Z <- conduct_ra(d_named)
  expect_true(all(as.character(Z) %in% c("ctrl", "trt")))
  expect_equal(sum(Z == "trt"), 3)

  expect_error(
    declare_ra(N = 6, num_arms = 2, conditions = c("A", "B", "C"),
               ra_type = "balanced"),
    "one entry per condition"
  )
  expect_error(
    declare_ra(N = 9, num_arms = 3, prob_each = c(0.5, 0.5),
               ra_type = "balanced"),
    "probabilities describe 2 conditions"
  )

  d3 <- declare_ra(N = 9, num_arms = 3, ra_type = "balanced")
  expect_equal(class(d3)[2], "ra_balanced")
  expect_equal(d3$num_arms, 3)
  expect_equal(d3$conditions, c("T1", "T2", "T3"))
  expect_equal(
    unname(d3$probabilities_matrix[1, ]),
    rep(1 / 3, 3)
  )
  expect_true(all(abs(d3$probabilities_matrix - 1 / 3) < 1e-12))
  set.seed(24)
  Z3 <- conduct_ra(d3)
  expect_true(all(Z3 %in% c("T1", "T2", "T3")))
  expect_equal(length(unique(Z3)), 3L)
  expect_equal(as.integer(table(Z3)), c(3L, 3L, 3L))

  d_named3 <- declare_ra(N = 9, conditions = c("A", "B", "C"),
                         ra_type = "balanced")
  expect_equal(class(d_named3)[2], "ra_balanced")
  expect_equal(d_named3$num_arms, 3)
  expect_equal(d_named3$conditions, c("A", "B", "C"))
  expect_equal(
    unname(d_named3$probabilities_matrix[1, ]),
    rep(1 / 3, 3)
  )
  set.seed(25)
  Zn <- conduct_ra(d_named3)
  expect_true(all(as.character(Zn) %in% c("A", "B", "C")))
  expect_equal(length(unique(Zn)), 3L)

  d3_pe <- declare_ra(N = 9, num_arms = 3, prob_each = rep(1 / 3, 3),
                      ra_type = "balanced")
  expect_equal(class(d3_pe)[2], "ra_balanced")
  expect_equal(d3_pe$num_arms, 3)
  expect_equal(unname(d3_pe$probabilities_matrix),
               unname(d3$probabilities_matrix))
})

test_that("count arguments and permutation_matrix are refused on the balanced path", {
  expect_error(declare_ra(N = 10, m_each = c(4, 6), ra_type = "balanced"),
               "not with")
  expect_error(declare_ra(N = 10, m_unit = rep(1, 10), ra_type = "balanced"),
               "not with")
  expect_error(
    declare_ra(blocks = rep(1:2, each = 5), block_m = c(2, 2),
               ra_type = "balanced"),
    "not with"
  )
  expect_error(
    declare_ra(blocks = rep(1:2, each = 5),
               block_m_each = rbind(c(2, 3), c(2, 3)),
               ra_type = "balanced"),
    "not with"
  )
  expect_error(
    declare_ra(blocks = rep(1:2, each = 5), block_prob = c(0.4, 0.6),
               ra_type = "balanced"),
    "not with"
  )
  expect_error(
    declare_ra(blocks = rep(1:2, each = 5),
               block_prob_each = rbind(c(0.4, 0.6), c(0.5, 0.5)),
               ra_type = "balanced"),
    "not with"
  )
  expect_error(
    declare_ra(N = 10, permutation_matrix = matrix(0:1, 10, 2),
               ra_type = "balanced"),
    "permutation_matrix"
  )
  expect_error(
    declare_ra(N = 6, prob = 0.4, prob_unit = rep(0.5, 6), ra_type = "balanced"),
    "only one of"
  )
})

test_that("declare_ra passes formula through to balanced_ra", {
  x <- c(0, 1, 5, 6, 8, 9)
  d <- declare_ra(formula = ~ x)
  expect_equal(class(d)[2], "ra_balanced")
  expect_true(inherits(d$formula, "formula"))
  set.seed(51)
  z_decl <- conduct_ra(d)
  set.seed(51)
  z_direct <- balanced_ra(formula = ~ x)
  expect_equal(z_decl, z_direct)
  expect_true(all(z_decl %in% 0:1))

  expect_error(
    declare_ra(prob = 0.5, formula = ~ x, blocks = rep(1:3, each = 2)),
    "Use B in the formula, or use blocks=, not both"
  )
})

