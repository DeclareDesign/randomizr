context("Condition Names")


test_that("Condition Names", {
  
  N <- 10
  complete_ra(10, 5)
  complete_ra(N = N, num_arms = 2)
  complete_ra(N = N, num_arms = 3)
  complete_ra(N = N, m = 3, conditions = c(T, F))
  complete_ra(N = N, m = 3, conditions = c(F, T))
  expect_error(complete_ra(100, conditions = c("control", "control", "treatment")))
  
  complete_ra(10, conditions = c("C", "B", "A"))
  

# factors -----------------------------------------------------------------


  fact_1 <- factor(c("A", "B"), levels = c("A", "B"))
  fact_2 <- factor(c("A", "B"), levels = c("B", "A"))
    
  complete_ra(N, conditions = fact_1)
  complete_ra(N, conditions = fact_2)
  
  
  
})