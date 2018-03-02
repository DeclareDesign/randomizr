context("Condition Names")


expect_u <- function(draw, l){
  expect_true(all(draw %in% l))
}

test_that("Condition Names", {
  
  N <- 10
  
  expect_u(complete_ra(N, 5), 0:1)
  

  expect_u(
    complete_ra(N = N, num_arms = 2), 
    c("T1", "T2")
  )
  
  expect_u(
    complete_ra(N = N, num_arms = 3), 
    c("T1", "T2", "T3")
  )
  
  expect_u(
    complete_ra(N = N, m = 3, conditions = c(T, F)),
    c(TRUE, FALSE)
  )
  
  expect_u(
    complete_ra(N = N, m = 3, conditions = c(F, T)),
    c(FALSE, TRUE)
  )
  
  expect_u(
    complete_ra(10, conditions = c("C", "B", "A")),
    c("C", "B", "A")
  )

  expect_error(complete_ra(100, conditions = c("control", "control", "treatment")))
})

# factors -----------------------------------------------------------------

test_that("Condition Names w/ factors", {
  N <- 32  
  
  fact_1 <- factor(c("A", "B"), levels = c("A", "B"))
  fact_2 <- factor(c("A", "B"), levels = c("B", "A"))
    
  expect_u(
    complete_ra(N, conditions = fact_1), 
    fact_1
  )
  
  expect_u(
    complete_ra(N, conditions = fact_2), 
    fact_2
  )

})