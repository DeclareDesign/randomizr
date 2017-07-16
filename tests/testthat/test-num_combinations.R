
randomizr:::complete_ra_num_permutations(6, prob_each = c(.5, .5), condition_names = c(0, 1))
choose(6, 3)
perms <- randomizr:::complete_ra_permutations(6, prob_each = c(.5, .5), condition_names = c(0, 1))
dim(perms)
perm_probs <- randomizr:::complete_ra_permutation_probabilities(6, prob_each = c(.5, .5), condition_names = c(0, 1))
length(perm_probs)
sum(perm_probs)

randomizr:::complete_ra_num_permutations(10, prob_each = c(.5, .5), condition_names = c(0, 1))
choose(10, 5)
perms <- randomizr:::complete_ra_permutations(10, prob_each = c(.5, .5), condition_names = c(0, 1))
dim(perms)
perm_probs <- randomizr:::complete_ra_permutation_probabilities(10, prob_each = c(.5, .5), condition_names = c(0, 1))
length(perm_probs)
sum(perm_probs)


randomizr:::complete_ra_num_permutations(3, prob_each = c(.5, .5), condition_names = c(0, 1))
choose(3, 1) + choose(3, 2)
perms <- randomizr:::complete_ra_permutations(3, prob_each = c(.5, .5), condition_names = c(0, 1))
dim(perms)
perm_probs <- randomizr:::complete_ra_permutation_probabilities(3, prob_each = c(.5, .5), condition_names = c(0, 1))
length(perm_probs)
sum(perm_probs)


randomizr:::complete_ra_num_permutations(10, prob_each = c(1/3, 1/3, 1/3), condition_names = c("T1", "T2", "T3"))
perms <- randomizr:::complete_ra_permutations(10, prob_each = c(1/3, 1/3, 1/3), condition_names = c("T1", "T2", "T3"))
dim(perms)
dim(unique(perms))
perm_probs <- randomizr:::complete_ra_permutation_probabilities(10, prob_each = c(1/3, 1/3, 1/3), condition_names = c("T1", "T2", "T3"))
length(perm_probs)
sum(perm_probs)


declaration <- declare_ra(N = 4)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)
obtain_permutation_probabilities(declaration)

# blocked -----------------------------------------------------------------

block_var <- c("A", "A", "B", "B", "C", "C", "C")
declaration <- declare_ra(block_var = block_var)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)
obtain_permutation_probabilities(declaration)


# clustered

clust_var <- c("A", "B", "A", "B", "C", "C", "C")
declaration <- declare_ra(clust_var = clust_var)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)
obtain_permutation_probabilities(declaration)


# test against RI package

y <- c(8, 6, 2, 0, 3, 1, 1, 1, 2, 2, 0, 1, 0, 2, 2, 4, 1, 1)
Z <- c(1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0)
cluster <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)
block <- c(rep(1, 4), rep(2, 6), rep(3, 8))

df <- data.frame(Z, cluster, block)

declaration <- declare_ra(
  block_var = df$block,
  clust_var = df$cluster,
  block_m = tapply(df$Z, df$block, sum) / 2
)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
obtain_num_permutations(declaration)

mine <- obtain_permutation_matrix(declaration = declaration)
rowMeans(mine) - declaration$probabilities_matrix[,2]
theirs <- ri::genperms(df$Z, df$block, df$cluster)

duplicated(mine, MARGIN = 2)
duplicated(theirs, MARGIN = 2)
duplicated(cbind(mine, theirs), MARGIN = 2)

# test big numbers

declaration <- declare_ra(20)
choose(20, 10)
perms <- obtain_permutation_matrix(declaration)
dim(perms)


# test wacky declarations

declaration <- declare_ra(N = 5, prob_each = c(.49, .51))
obtain_num_permutations(declaration)
perm_probs <- obtain_permutation_probabilities(declaration)
perms <- obtain_permutation_matrix(declaration)
declaration$probabilities_matrix[,2]

# correctly WRONG because the perms have different probs!
rowMeans(perms)

# correctly correct!
perms %*% perm_probs


