context("Condition Names")


test_that("Condition Names", {
  
  N <- 10
  complete_ra(10, 5)
  complete_ra(N = N, num_arms = 2)
  complete_ra(N = N, num_arms = 3)
  complete_ra(N = N, m = 3, condition_names = c(T, F))
  complete_ra(N = N, m = 3, condition_names = c(F, T))
  expect_error(complete_ra(100, condition_names = c("control", "control", "treatment")))
  
  complete_ra(10, condition_names = c("C", "B", "A"))
  

# factors -----------------------------------------------------------------


  fact_1 <- factor(c("A", "B"), levels = c("A", "B"))
  fact_2 <- factor(c("A", "B"), levels = c("B", "A"))
    
  complete_ra(N, condition_names = fact_1)
  complete_ra(N, condition_names = fact_2)
  
  
  
})