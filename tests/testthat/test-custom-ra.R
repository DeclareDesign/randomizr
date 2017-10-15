
context("Custom Random Assignments")

declaration <- declare_ra(N = 10, num_arms = 4)
permutation_matrix <- obtain_permutation_matrix(declaration, maximum_permutations = 100)

declaration_2 <- declare_ra(permutation_matrix = permutation_matrix)


declaration_2$probabilities_matrix
Z <- declaration_2$ra_function()


obtain_condition_probabilities(declaration = declaration_2, assignment = Z)
obtain_permutation_matrix(declaration_2)
obtain_num_permutations(declaration_2)


