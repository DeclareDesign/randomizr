

context("Custom Random Assignments")

test_that("custom RA yields inputs", {
  declaration <- declare_ra(N = 10, num_arms = 4)
  permutation_matrix <-
    obtain_permutation_matrix(declaration, maximum_permutations = 100)
  
  declaration_2 <-
    declare_ra(permutation_matrix = permutation_matrix)
  
  prob <- declaration_2$probabilities_matrix
  expect_equal(dim(prob), c(10, 4))
  
  Z <- conduct_ra(declaration_2)
  
  expect_identical(permutation_matrix,
                   obtain_permutation_matrix(declaration_2))
  
  expect_identical(obtain_condition_probabilities(declaration = declaration_2, assignment = Z),
                   prob[cbind(seq_along(Z), match(paste0("prob_", Z), colnames(prob)))])
  
  
  expect_equal(obtain_num_permutations(declaration_2),
               ncol(permutation_matrix))
  
})
