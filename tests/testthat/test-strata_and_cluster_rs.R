context("Stratified and Clustered Random Sampling")

clust_var <- rep(letters, times = 1:26)
strata_var <- rep(NA, length(clust_var))
strata_var[clust_var %in% letters[1:5]] <- "stratum_1"
strata_var[clust_var %in% letters[6:10]] <- "stratum_2"
strata_var[clust_var %in% letters[11:15]] <- "stratum_3"
strata_var[clust_var %in% letters[16:20]] <- "stratum_4"
strata_var[clust_var %in% letters[21:26]] <- "stratum_5"

table(strata_var, clust_var)

S <- strata_and_cluster_rs(clust_var = clust_var, strata_var = strata_var)
probs <- strata_and_cluster_rs_probabilities(clust_var = clust_var, strata_var = strata_var)

table(S, clust_var)
table(S, strata_var)
table(probs, clust_var)
table(probs, strata_var)

S <- strata_and_cluster_rs(clust_var = clust_var,
                           strata_var = strata_var,
                           prob = .5)
probs <- strata_and_cluster_rs_probabilities(clust_var = clust_var, strata_var = strata_var)

table(S, clust_var)
table(S, strata_var)
table(probs, clust_var)
table(probs, strata_var)

S <- strata_and_cluster_rs(clust_var = clust_var,
                           strata_var = strata_var,
                           prob = 0)
probs <- strata_and_cluster_rs_probabilities(clust_var = clust_var,
                                             strata_var = strata_var,
                                             prob = 0)


table(S, clust_var)
table(S, strata_var)
table(probs, clust_var)
table(probs, strata_var)

S <- strata_and_cluster_rs(clust_var = clust_var, 
                           strata_var = strata_var,
                           prob = 1)
probs <- strata_and_cluster_rs_probabilities(clust_var = clust_var,
                                             strata_var = strata_var,
                                             prob = 1)

table(S, clust_var)
table(S, strata_var)
table(probs, clust_var)
table(probs, strata_var)

S <- strata_and_cluster_rs(clust_var = clust_var,
                           strata_var = strata_var,
                           strata_n = c(2, 3, 2, 3, 2)
                           )

probs <- strata_and_cluster_rs_probabilities(clust_var = clust_var,
                                             strata_var = strata_var,
                                             strata_n = c(2, 3, 2, 3, 2))

table(S, clust_var)
table(S, strata_var)
table(probs, clust_var)
table(probs, strata_var)

S <- strata_and_cluster_rs(clust_var = clust_var,
                           strata_var = strata_var,
                           strata_prob = c(.1, .2, .3, .4, .5)
                           )

probs <- strata_and_cluster_rs_probabilities(clust_var = clust_var,
                                             strata_var = strata_var,
                                             strata_prob = c(.1, .2, .3, .4, .5))
table(S, clust_var)
table(S, strata_var)
table(probs, clust_var)
table(probs, strata_var)


