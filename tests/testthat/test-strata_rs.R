
context("Stratified Random Sampling")

strata_var <- rep(c("A", "B","C"), times=c(50, 100, 200))


# Errors ------------------------------------------------------------------

expect_error(strata_rs(strata_var=strata_var, strata_prob = c(.1, .2, .3, .4)))
expect_error(strata_rs(strata_var=strata_var, strata_prob = c(.1, .2, -.3)))
expect_error(strata_rs(strata_var=strata_var, strata_prob = c(.1, .2, 1.1)))
expect_error(strata_rs(strata_var=strata_var, strata_n = c(10, 101, 40)))
expect_error(strata_rs(strata_var=strata_var, prob = 1.2))
expect_error(strata_rs(strata_var=strata_var, prob = -1))

S <- strata_rs(strata_var = strata_var)
table(strata_var, S)
probs <- strata_rs_probabilities(strata_var = strata_var)
table(strata_var, probs)


S <- strata_rs(strata_var = strata_var, prob = .5)
table(strata_var, S)
probs <- strata_rs_probabilities(strata_var = strata_var, prob = .5)
table(strata_var, probs)

S <- strata_rs(strata_var = strata_var, prob = 1)
table(strata_var,S)
probs <- strata_rs_probabilities(strata_var = strata_var, prob = 1)
table(strata_var,probs)


S <- strata_rs(strata_var = strata_var, prob = 0)
table(strata_var,S)
probs <- strata_rs_probabilities(strata_var = strata_var, prob = 0)
table(strata_var,probs)


S <- strata_rs(strata_var = strata_var, prob = .33)
table(strata_var,S)
probs <- strata_rs_probabilities(strata_var = strata_var, prob = .33)
table(strata_var,probs)

S <- strata_rs(strata_var=strata_var, strata_n = c(20, 30, 40))
table(strata_var, S)
probs <- strata_rs_probabilities(strata_var = strata_var, strata_n = c(20, 30, 40))
table(strata_var,probs)

S <- strata_rs(strata_var=strata_var, strata_prob = c(.1, .2, .3))
table(strata_var, S)
probs <- strata_rs_probabilities(strata_var = strata_var, strata_prob = c(.1, .2, .3))
table(strata_var,probs)

S <- strata_rs(strata_var=strata_var, strata_prob = c(0, .2, .3))
table(strata_var, S)
probs <- strata_rs_probabilities(strata_var = strata_var, strata_prob = c(0, .2, .3))
table(strata_var,probs)

