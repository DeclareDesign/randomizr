
context("Blocked and Clustered Random Assignments")

clusters <- rep(letters, times=1:26)
blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:5]] <- "block_1"
blocks[clusters %in% letters[6:10]] <- "block_2"
blocks[clusters %in% letters[11:15]] <- "block_3"
blocks[clusters %in% letters[16:20]] <- "block_4"
blocks[clusters %in% letters[21:26]] <- "block_5"

table(blocks, clusters)

df <- data.frame(blocks, clusters)
df <- df[sample(1:nrow(df)),]

Z <- block_and_cluster_ra(clusters = df$clusters, blocks = df$blocks)

table(Z, df$clusters)
table(Z, df$blocks)

Z <- block_and_cluster_ra(clusters = clusters, blocks = blocks, num_arms = 3)

table(Z, clusters)
table(Z, blocks)

Z <- block_and_cluster_ra(clusters = clusters, blocks = blocks, prob_each = c(.2, .5, .3))
Z <- block_and_cluster_ra(clusters = clusters, blocks = blocks, prob_each = c(.2, .8))
Z <- block_and_cluster_ra(clusters = clusters, blocks = blocks, num_arms = 2)

Z <- block_and_cluster_ra(clusters = clusters, blocks = blocks, m = 2)
table(Z, clusters)
table(Z, blocks)

table(Z, clusters, blocks)



block_m_each <- rbind(c(2, 3),
                 c(1, 4),
                 c(3, 2),
                 c(2, 3),
                 c(5, 1))

Z <- block_and_cluster_ra(clusters = clusters, blocks = blocks, block_m_each = block_m_each)

table(Z, clusters)
table(Z, blocks)



Z <- block_and_cluster_ra(clusters = clusters, blocks = blocks, prob = .5)

table(Z, clusters)
table(Z, blocks)

Z <- block_and_cluster_ra(clusters = clusters, blocks = blocks, prob = 0)

table(Z, clusters)
table(Z, blocks)

Z <- block_and_cluster_ra(clusters = clusters, blocks = blocks, prob = 1)

table(Z, clusters)
table(Z, blocks)


