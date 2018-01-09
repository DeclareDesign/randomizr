context("Probabilities: Complete")

# 2-arm designs
complete_ra_probabilities(N = 100)
complete_ra_probabilities(N = 100, m = 50)
complete_ra_probabilities(N = 100, prob = .3)

complete_ra_probabilities(
  N = 100,
  m_each = c(30, 70),
  condition_names = c("control", "treatment")
)

# Multi-arm Designs
complete_ra_probabilities(N = 100, num_arms = 3)
complete_ra_probabilities(N = 100, m_each = c(30, 30, 40))

complete_ra_probabilities(
  N = 100,
  m_each = c(30, 30, 40),
  condition_names = c("control", "placebo", "treatment")
)

complete_ra_probabilities(N = 100,
                          condition_names = c("control", "placebo", "treatment"))
complete_ra_probabilities(N = 100, prob_each = c(.2, .7, .1))

complete_ra_probabilities(N = 101, prob_each = c(.2, .7, .1))

#compare_to_empirical <- replicate(10000, complete_ra(N=103, prob_each = c(.2, .7, .1)))

#mean(compare_to_empirical=="T1")
#mean(compare_to_empirical=="T2")
#mean(compare_to_empirical=="T3")

# Special cases

complete_ra(2,
            m_each = c(1, 0, 1),
            condition_names = c("T1", "T2", "T3"))
complete_ra_probabilities(2,
                          m_each = c(1, 0, 1),
                          condition_names = c("T1", "T2", "T3"))
complete_ra_probabilities(2,
                          m_each = c(0, 0, 2),
                          condition_names = c("T1", "T2", "T3"))



# Simple Designs ----------------------------------------------------------

context("Probabilities: Simple")

# Two Group Designs
simple_ra_probabilities(N = 100)
simple_ra_probabilities(N = 100, prob = 0.5)
simple_ra_probabilities(
  N = 100,
  prob_each = c(0.3, 0.7),
  condition_names = c("control", "treatment")
)
# Multi-arm Designs
simple_ra_probabilities(N = 100, num_arms = 3)
simple_ra_probabilities(N = 100, prob_each = c(0.3, 0.3, 0.4))
simple_ra_probabilities(
  N = 100,
  prob_each = c(0.3, 0.3, 0.4),
  condition_names = c("control", "placebo", "treatment")
)
simple_ra_probabilities(N = 100,
                        condition_names = c("control", "placebo", "treatment"))


# Blocked Designs ---------------------------------------------------------

context("Probabilities: Blocked")
blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))
block_ra_probabilities(blocks = blocks)

block_m_each <- rbind(c(25, 25),
                      c(50, 50),
                      c(100, 100))

block_ra_probabilities(blocks = blocks, block_m_each = block_m_each)

block_m_each <- rbind(c(10, 40),
                      c(30, 70),
                      c(50, 150))

block_ra_probabilities(
  blocks = blocks,
  block_m_each = block_m_each,
  condition_names = c("control", "treatment")
)

block_ra_probabilities(blocks = blocks, num_arms = 3)
#Z_mat <- replicate(10000, block_ra(blocks=blocks, num_arms=3))

block_m_each <- rbind(c(10, 20, 20),
                      c(30, 50, 20),
                      c(50, 75, 75))
block_ra_probabilities(blocks = blocks, block_m_each = block_m_each)

block_ra_probabilities(
  blocks = blocks,
  block_m_each = block_m_each,
  condition_names = c("control", "placebo", "treatment")
)

block_ra_probabilities(blocks = blocks, prob_each = c(.1, .1, .8))

block_prob_each <- rbind(c(.1, .2, .7),
                         c(.1, .3, .6),
                         c(1 / 3, 1 / 3, 1 / 3))

block_ra_probabilities(blocks = blocks, block_prob_each = block_prob_each)

#Z_mat <- replicate(10000, block_ra(blocks=blocks, block_prob_each=block_prob_each))
#prop.table(table(Z_mat[blocks == "A"]))
#prop.table(table(Z_mat[blocks == "B"]))
#prop.table(table(Z_mat[blocks == "C"]))

context("Probabilities: Clustered")
# clustered designs -------------------------------------------------------


#debugonce(cluster_ra_probabilities)
# Two Group Designs
clusters <- rep(letters, times = 1:26)
cluster_ra_probabilities(clusters = clusters)

cluster_ra_probabilities(clusters = clusters, m = 10)

cluster_ra_probabilities(
  clusters = clusters,
  m_each = c(9, 17),
  condition_names = c("control", "treatment")
)

# Multi-arm Designs
cluster_ra_probabilities(clusters = clusters, num_arms = 3)
cluster_ra_probabilities(clusters = clusters, m_each = c(7, 7, 12))

cluster_ra_probabilities(
  clusters = clusters,
  m_each = c(7, 7, 12),
  condition_names = c("control", "placebo", "treatment")
)

cluster_ra_probabilities(
  clusters = clusters,
  condition_names = c("control", "placebo", "treatment")
)

cluster_ra_probabilities(clusters = clusters, prob_each = c(.1, .2, .7))


context("Probabilities: Blocked and Clustered")
# Blocked and clustered designs -------------------------------------------
clusters <- rep(letters, times = 1:26)
blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:5]] <- "block_1"
blocks[clusters %in% letters[6:10]] <- "block_2"
blocks[clusters %in% letters[11:15]] <- "block_3"
blocks[clusters %in% letters[16:20]] <- "block_4"
blocks[clusters %in% letters[21:26]] <- "block_5"

block_and_cluster_ra_probabilities(clusters = clusters, blocks = blocks)
block_and_cluster_ra_probabilities(clusters = clusters,
                                   blocks = blocks,
                                   num_arms = 3)
block_and_cluster_ra_probabilities(
  clusters = clusters,
  blocks = blocks,
  prob_each = c(.2, .5, .3)
)

block_m_each <- rbind(c(2, 3),
                      c(1, 4),
                      c(3, 2),
                      c(2, 3),
                      c(5, 1))

block_and_cluster_ra_probabilities(clusters = clusters,
                                   blocks = blocks,
                                   block_m_each = block_m_each)
