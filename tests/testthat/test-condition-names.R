context("Condition Names")


test_that("Condition Names", {
  
  N <- 10
  complete_ra(10, 5)
  complete_ra(N = N, num_arms = 2)
  complete_ra(N = N, num_arms = 3)
  complete_ra(N = N, m = 3, condition_names = c(T, F))
  complete_ra(N = N, m = 3, condition_names = c(F, T))
  expect_error(complete_ra(100, condition_names = c("control", "control", "treatment")))
  
  
  
  
  
  
  
})