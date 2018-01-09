context("Stratified and Clustered Random Sampling")

clusters <- rep(letters, times = 1:26)
strata <- rep(NA, length(clusters))
strata[clusters %in% letters[1:5]] <- "stratum_1"
strata[clusters %in% letters[6:10]] <- "stratum_2"
strata[clusters %in% letters[11:15]] <- "stratum_3"
strata[clusters %in% letters[16:20]] <- "stratum_4"
strata[clusters %in% letters[21:26]] <- "stratum_5"

table(strata, clusters)

S <- strata_and_cluster_rs(clusters = clusters, strata = strata)
probs <- strata_and_cluster_rs_probabilities(clusters = clusters, strata = strata)

S <- strata_and_cluster_rs(clusters = clusters, strata = strata, n = 2)
probs <- strata_and_cluster_rs_probabilities(clusters = clusters, strata = strata, n = 2)

table(S, clusters)
table(S, strata)
table(S, clusters, strata)
table(probs, clusters)
table(probs, strata)
table(probs, clusters, strata)

S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           prob = .5)
probs <- strata_and_cluster_rs_probabilities(clusters = clusters, strata = strata)

table(S, clusters)
table(S, strata)
table(probs, clusters)
table(probs, strata)

S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           prob = 0)
probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
                                             strata = strata,
                                             prob = 0)


table(S, clusters)
table(S, strata)
table(probs, clusters)
table(probs, strata)

S <- strata_and_cluster_rs(clusters = clusters, 
                           strata = strata,
                           prob = 1)
probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
                                             strata = strata,
                                             prob = 1)

table(S, clusters)
table(S, strata)
table(probs, clusters)
table(probs, strata)

S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           strata_n = c(2, 3, 2, 3, 2)
                           )

probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
                                             strata = strata,
                                             strata_n = c(2, 3, 2, 3, 2))

table(S, clusters)
table(S, strata)
table(probs, clusters)
table(probs, strata)

S <- strata_and_cluster_rs(clusters = clusters,
                           strata = strata,
                           strata_prob = c(.1, .2, .3, .4, .5)
                           )

probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
                                             strata = strata,
                                             strata_prob = c(.1, .2, .3, .4, .5))
table(S, clusters)
table(S, strata)
table(probs, clusters)
table(probs, strata)


