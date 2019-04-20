context("Complete Random Assignments")

test_that("Invalid complete RAs", {
  expect_error(complete_ra(100, prob_each = c(.2)))
  expect_error(complete_ra(100, m_each = c(30, 70), prob_each = c(.3, .7)))
  expect_error(complete_ra(100, m_each = c(20, 20, 20)))
  expect_error(complete_ra(100, m_each = c(20, 20, 60), conditions = 1:2))
  expect_error(complete_ra(100, prob_each = c(.2, .7)))
  expect_error(complete_ra(N = 100, m = 101))
  expect_error(complete_ra(N = 100, m = -2))
  expect_error(complete_ra(N = 0, m = 0))
  expect_error(complete_ra(N = 1, m = 2))
})

test_that("N=100, m=3", {
  expect_identical(table(complete_ra(
    100,
    m = 30,
    conditions = c("Control", "Treatment")
  )),
  structure(
    c(70L, 30L),
    .Dim = 2L,
    .Dimnames = structure(list(c(
      "Control", "Treatment"
    )), .Names = ""),
    class = "table"
  ))
})


# should this be allowed?
test_that("num_arms=1", {
  expect_equal(complete_ra(100, num_arms = 1),
               factor(rep("T1", 100)))
})

test_that("m_each = N", {
  expect_equal(complete_ra(100, m_each = c(100)),
               factor(rep("T1", 100)))
})

test_that("length(conditions) = 1", {
  expect_equal(complete_ra(100, conditions = c("Treatment")),
               factor(rep("Treatment", 100)))
})

test_that("N=1 num_arms=2", {
  expect_equivalent(as.numeric(sort(table(
    complete_ra(1, num_arms = 2)
  ))),
  0:1)
  
  expect_equivalent(as.numeric(table(
    complete_ra(1, num_arms = 2, conditions = 0:1)
  )),
  1)
})

test_that("N=100", {
  Z <- complete_ra(N = 100)
  expect_equal(sum(Z), 50)
})

test_that("prob=.34", {
  Z <- complete_ra(N = 101, prob = .34)
  expect_true(table(Z)[2] %in% 34:35)
})

test_that("prob_each = .34,.66", {
  Z <- complete_ra(N = 101, prob_each = c(.34, .66))
  expect_true(table(Z)[1] %in% 34:35)
})

test_that("50/50", {
  Z <- complete_ra(N = 100, m = 50)
  expect_equal(sum(Z), 50)
})

test_that("30/70 named", {
  Z <- complete_ra(
    N = 100,
    m_each = c(30, 70),
    conditions = c("control", "treatment")
  )
  expect_equivalent(as.numeric(table(Z)), c(30, 70))
})

test_that("3 arms", {
  # Multi-arm Designs
  Z <- complete_ra(N = 100, num_arms = 3)
  expect_true(all(table(Z) %in% c(33, 34)))
})

test_that("30/30/40", {
  Z <- complete_ra(N = 100, m_each = c(30, 30, 40))
  expect_equivalent(as.numeric(table(Z)), c(30, 30, 40))
})

test_that("30/30/40 with named conditions", {
  Z <- complete_ra(
    N = 100,
    m_each = c(30, 30, 40),
    conditions = c("control", "placebo", "treatment")
  )
  expect_equivalent(as.numeric(table(Z)), c(30, 30, 40))
})

test_that("3 named conditions", {
  Z <- complete_ra(N = 100,
                   conditions = c("control", "placebo", "treatment"))
  
  expect_true(all(table(Z) %in% c(33, 34)))
})

test_that("zero in m_each", {
  Z <- complete_ra(2,
                   m_each = c(1, 0, 1),
                   conditions = c("T1", "T2", "T3"))
  expect_true(all(Z != "T2"))
})

test_that("Not biased", {
  expect_lt(sum(replicate(1000, complete_ra(1))), 600)
  expect_gt(sum(replicate(1000, complete_ra(1))), 400)
  # correct!
  replicate(100, complete_ra(N = 1, m = 1))
  
  for (N in 1:5) {
    expect_equal(sum(replicate(100, complete_ra(
      N = N, m = 0
    ))),
    0)
  }
  
  expect_equal(complete_ra(N = 2, m = 2), c(1, 1))
})

test_that("zeros in m_each", {
  Z <- complete_ra(N = 100, m_each = c(50, 50, 0))
  expect_equivalent(as.numeric(table(Z)), c(50, 50, 0))
  
  Z <- complete_ra(N = 100, m_each = c(100, 0, 0))
  expect_equivalent(as.numeric(table(Z)), c(100, 0, 0))
  
  conditions <- factor(c("T1", "T2"), levels = c("T1", "T2", "T3"))
  Z <- complete_ra(
    N = 67,
    prob_each = c(.5, .5),
    conditions = conditions,
    check_inputs = FALSE
  )
  expect_equivalent(table(Z)["T3"], 0)
})

test_that("N=1 handling", {
  expect_error(S <- complete_ra(N = 1, m = .5))
  S <- complete_ra(N = 1, prob = .2)
  
  #expect_equivalent(complete_ra_probabilities(N = 1, m = 1), c(.5, .5))
  expect_equivalent(complete_ra_probabilities(N = 1, m = 1), c(0, 1))
  expect_equivalent(complete_ra_probabilities(N = 1, m = 0), c(1, 0))
  
  expect_equivalent(complete_ra_probabilities(N = 1), c(.5, .5))
  expect_equivalent(complete_ra_probabilities(N = 1, prob = .2), c(.8, .2))
  
  expect_error(complete_ra_probabilities(N = 1, m = .5))
  
  expect_true(all(c(0, 1) %in% replicate(100, complete_ra(N = 1))))
  expect_true(all(replicate(100, complete_ra(
    N = 1, m = 1
  )) == 1))
  expect_true(all(replicate(100, complete_ra(
    N = 1, m = 0
  )) == 0))
  
})

test_that("N=2 roundup rule", {
  expect_equivalent(complete_ra_probabilities(N = 2, prob = .95),
                    c(.5, .5, .5, .5))
})


test_that("multi-dim fixup", {
  ra <- declare_ra(N = 4, prob_each = c(1, 1, 1, 2) / 5)
  expect_length(table(obtain_permutation_probabilities(ra)), 7)
})

test_that("_unit",{
  table(complete_ra(100, prob_unit = rep(0.4, 100)))
  expect_error(table(complete_ra(100, prob = rep(0.4, 100))))
  expect_error(complete_ra(100, prob_unit = rep(c(0.4, 0.5), c(50, 50))))
  
  table(complete_ra(100, m_unit = rep(40, 100)))
  expect_error(complete_ra(100, m = rep(40, 100)), "scalar.")
  expect_error(complete_ra(100, m_unit = rep(c(40, 50), c(50, 50))), "In a complete random assignment design, `m_unit` must be the same for all units.")
})

