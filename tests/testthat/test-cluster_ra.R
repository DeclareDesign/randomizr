
context("Cluster Random Assignments")

# Two Group Designs
clusters <- rep(letters, times=1:26)
Z <- cluster_ra(clusters=clusters)
table(Z, clusters)

Z <- cluster_ra(clusters=clusters, m=13)
table(Z, clusters)
Z <- cluster_ra(clusters=clusters, m_each = c(10, 16), 
                conditions = c("control", "treatment"))

Z <- cluster_ra(clusters=clusters, prob = .5)
table(Z, clusters)
Z <- cluster_ra(clusters=clusters, prob = 0)
table(Z, clusters)
Z <- cluster_ra(clusters=clusters, prob = 1)
table(Z, clusters)

# Multi-arm Designs
Z <- cluster_ra(clusters=clusters, num_arms=3)
table(Z, clusters)

Z <- cluster_ra(clusters=clusters, m_each=c(7, 7, 12))
table(Z, clusters)

Z <- cluster_ra(clusters=clusters, m_each=c(7, 7, 12), 
                conditions=c("control", "placebo", "treatment"))
table(Z, clusters)

Z <- cluster_ra(clusters=clusters, 
                conditions=c("control", "placebo", "treatment"))
table(Z, clusters)

Z <- cluster_ra(clusters=clusters, prob_each = c(.1, .2, .7))
table(Z, clusters)

