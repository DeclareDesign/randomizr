
context("Cluster Random Sampling")

#debugonce(cluster_rs_probabilities)
# Two Group Designs
clust_var <- rep(letters, times = 1:26)
S <- cluster_rs(clust_var = clust_var)
table(S, clust_var)
probs <- cluster_rs_probabilities(clust_var = clust_var)
table(probs, clust_var)

S <- cluster_rs(clust_var = clust_var, n = 13)
table(S, clust_var)
probs <- cluster_rs_probabilities(clust_var = clust_var, n = 13)
table(probs, clust_var)

S <- cluster_rs(clust_var = clust_var, prob = .5)
table(S, clust_var)
probs <- cluster_rs_probabilities(clust_var = clust_var, prob = .5)
table(probs, clust_var)

S <- cluster_rs(clust_var = clust_var, prob = 0)
table(S, clust_var)
probs <- cluster_rs_probabilities(clust_var = clust_var, prob = 0)
table(probs, clust_var)

S <- cluster_rs(clust_var = clust_var, prob = 1)
table(S, clust_var)
probs <- cluster_rs_probabilities(clust_var = clust_var, prob = 1)
table(probs, clust_var)
