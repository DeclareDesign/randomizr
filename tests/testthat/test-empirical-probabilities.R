library(randomizr)

# Test against empiricial probabilities -----------------------------------

block_var <- c("A", "A", "A", "B", "B", "C")
prob_each <- c(.1, .2, .7)

analytic_probs <- 
  rbind(
    c(
      0/2 * 2/3 + 1/3*1/3,
      0/2 * 2/3 + 1/3*1/3,
      2/2 * 2/3 + 1/3*1/3
    ),
    c(
      0/1 * 1/2 + 1/3*1/2,
      0/1 * 1/2 + 1/3*1/2,
      1/1 * 1/2 + 1/3*1/2
    ),
    c(
      0/1 * 0/1 + 1/3*1/1,
      0/1 * 0/1 + 1/3*1/1,
      0/1 * 0/1 + 1/3*1/1
    )
  )


# construct analytic probs from randomizr arguments
num_arms <- length(prob_each)
N_per_block <- table(block_var)

for_sure_allocations <- floor(N_per_block %*% t(prob_each))
possible_allocations <- matrix(1/num_arms, nrow = length(N_per_block), ncol = num_arms)

for_sure_probs <- sweep(for_sure_allocations, 1, N_per_block, `/`)
possible_probs <- sweep(possible_allocations, 1, (N_per_block - rowSums(for_sure_allocations))/N_per_block, `*`)
final_probs <- for_sure_probs + possible_probs

# 
# # Conduct Simulation
# perm_mat <- replicate(1000, block_ra(block_var = block_var, prob_each = prob_each))
# 
# prop.table(table(perm_mat))
# prob_T1 <- apply(perm_mat, 1, function(x)mean(x=="T1"))
# prob_T2 <- apply(perm_mat, 1, function(x)mean(x=="T2"))
# prob_T3 <- apply(perm_mat, 1, function(x)mean(x=="T3"))
# 
# simulated_probs <- 
# cbind(
#   tapply(prob_T1, block_var, mean),
#   tapply(prob_T2, block_var, mean),
#   tapply(prob_T3, block_var, mean)
# )
# 
# 
# simulated_probs


dec <- declare_ra(block_var = block_var, prob_each = prob_each)
block_ra_probabilities(block_var = block_var, prob_each = prob_each)

dec$probabilities_matrix

analytic_probs
final_probs



# figure out prob_each ----------------------------------------------------



prob <- 0.1
N <- 11
m_floor <- floor(N*prob)
m_ceiling <- ceiling(N*prob)

m_floor * (1-prob) + m_ceiling * (prob)
(prob * N)

prob_each <- c(.1, .2, .7)
m_each <- floor(N*prob_each)

m_each / (sum(m_each))
  


# complete_ra -------------------------------------------------------------

complete_ra(5, prob = .1)
complete_ra_probabilities(5, prob = .1)

Z_mat <- replicate(n = 10000, complete_ra(5, prob = .1))
mean(Z_mat)

ceiling(5*.1)
floor(5*.1)




