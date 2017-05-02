
context("Cluster Random Assignments")

# Two Group Designs
clust_var <- rep(letters, times=1:26)
Z <- cluster_ra(clust_var=clust_var)
table(Z, clust_var)

Z <- cluster_ra(clust_var=clust_var, m=13)
table(Z, clust_var)
Z <- cluster_ra(clust_var=clust_var, m_each = c(10, 16), 
                condition_names = c("control", "treatment"))

Z <- cluster_ra(clust_var=clust_var, prob = .5)
table(Z, clust_var)
Z <- cluster_ra(clust_var=clust_var, prob = 0)
table(Z, clust_var)
Z <- cluster_ra(clust_var=clust_var, prob = 1)
table(Z, clust_var)

# Multi-arm Designs
Z <- cluster_ra(clust_var=clust_var, num_arms=3)
table(Z, clust_var)

Z <- cluster_ra(clust_var=clust_var, m_each=c(7, 7, 12))
table(Z, clust_var)

Z <- cluster_ra(clust_var=clust_var, m_each=c(7, 7, 12), 
                condition_names=c("control", "placebo", "treatment"))
table(Z, clust_var)

Z <- cluster_ra(clust_var=clust_var, 
                condition_names=c("control", "placebo", "treatment"))
table(Z, clust_var)

Z <- cluster_ra(clust_var=clust_var, prob_each = c(.1, .2, .7))
table(Z, clust_var)

