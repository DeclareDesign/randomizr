

context("Stratified Random Sampling")

strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))


# Errors ------------------------------------------------------------------

expect_error(strata_rs(strata = strata, strata_prob = c(.1, .2, .3, .4)))
expect_error(strata_rs(strata = strata, strata_prob = c(.1, .2,-.3)))
expect_error(strata_rs(strata = strata, strata_prob = c(.1, .2, 1.1)))
expect_error(strata_rs(strata = strata, strata_n = c(10, 101, 40)))
expect_error(strata_rs(strata = strata, prob = 1.2))
expect_error(strata_rs(strata = strata, prob = -1))

S <- strata_rs(strata = strata)
table(strata, S)
probs <- strata_rs_probabilities(strata = strata)
table(strata, probs)


S <- strata_rs(strata = strata, prob = .5)
table(strata, S)
probs <- strata_rs_probabilities(strata = strata, prob = .5)
table(strata, probs)

S <- strata_rs(strata = strata, prob = 1)
table(strata, S)
probs <- strata_rs_probabilities(strata = strata, prob = 1)
table(strata, probs)


S <- strata_rs(strata = strata, prob = 0)
table(strata, S)
probs <- strata_rs_probabilities(strata = strata, prob = 0)
table(strata, probs)


S <- strata_rs(strata = strata, prob = .33)
table(strata, S)
probs <- strata_rs_probabilities(strata = strata, prob = .33)
table(strata, probs)

S <- strata_rs(strata = strata, strata_n = c(20, 30, 40))
table(strata, S)
probs <-
  strata_rs_probabilities(strata = strata, strata_n = c(20, 30, 40))
table(strata, probs)

S <- strata_rs(strata = strata, strata_prob = c(.1, .2, .3))
table(strata, S)
probs <-
  strata_rs_probabilities(strata = strata, strata_prob = c(.1, .2, .3))
table(strata, probs)

S <- strata_rs(strata = strata, strata_prob = c(0, .2, .3))
table(strata, S)
probs <-
  strata_rs_probabilities(strata = strata, strata_prob = c(0, .2, .3))
table(strata, probs)



test_that("Unhandled cases warn", {
  expect_warning(strata_rs_probabilities(
    strata = gl(2, 999),
    strata_prob = c("0.5", "0.5")
  ))
  
})
