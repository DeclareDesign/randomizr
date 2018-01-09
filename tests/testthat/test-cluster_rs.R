
context("Cluster Random Sampling")

#debugonce(cluster_rs_probabilities)
# Two Group Designs
clusters <- rep(letters, times = 1:26)
S <- cluster_rs(clusters = clusters)
table(S, clusters)
probs <- cluster_rs_probabilities(clusters = clusters)
table(probs, clusters)

S <- cluster_rs(clusters = clusters, n = 13)
table(S, clusters)
probs <- cluster_rs_probabilities(clusters = clusters, n = 13)
table(probs, clusters)

S <- cluster_rs(clusters = clusters, prob = .5)
table(S, clusters)
probs <- cluster_rs_probabilities(clusters = clusters, prob = .5)
table(probs, clusters)

S <- cluster_rs(clusters = clusters, prob = 0)
table(S, clusters)
probs <- cluster_rs_probabilities(clusters = clusters, prob = 0)
table(probs, clusters)

S <- cluster_rs(clusters = clusters, prob = 1)
table(S, clusters)
probs <- cluster_rs_probabilities(clusters = clusters, prob = 1)
table(probs, clusters)
