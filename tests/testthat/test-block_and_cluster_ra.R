library(testthat)
library(randomizr)

context("Blocked and Clustered Random Assignments")

clust_var <- rep(letters, times=1:26)
block_var <- rep(NA, length(clust_var))
block_var[clust_var %in% letters[1:5]] <- "block_1"
block_var[clust_var %in% letters[6:10]] <- "block_2"
block_var[clust_var %in% letters[11:15]] <- "block_3"
block_var[clust_var %in% letters[16:20]] <- "block_4"
block_var[clust_var %in% letters[21:26]] <- "block_5"

table(block_var, clust_var)

Z <- block_and_cluster_ra(clust_var = clust_var, block_var = block_var)

table(Z, clust_var)
table(Z, block_var)

Z <- block_and_cluster_ra(clust_var = clust_var, block_var = block_var, num_arms = 3)

table(Z, clust_var)
table(Z, block_var)

Z <- block_and_cluster_ra(clust_var = clust_var, block_var = block_var, prob_each = c(.2, .5, .3))


block_m_each <- rbind(c(2, 3),
                 c(1, 4),
                 c(3, 2),
                 c(2, 3),
                 c(5, 1))

Z <- block_and_cluster_ra(clust_var = clust_var, block_var = block_var, block_m_each = block_m_each)

table(Z, clust_var)
table(Z, block_var)



Z <- block_and_cluster_ra(clust_var = clust_var, block_var = block_var, prob = .5)

table(Z, clust_var)
table(Z, block_var)

Z <- block_and_cluster_ra(clust_var = clust_var, block_var = block_var, prob = 0)

table(Z, clust_var)
table(Z, block_var)

Z <- block_and_cluster_ra(clust_var = clust_var, block_var = block_var, prob = 1)

table(Z, clust_var)
table(Z, block_var)


