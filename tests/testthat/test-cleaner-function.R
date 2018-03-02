context("Cleaner Function")


test_that("checking randomizr argumnets", {
  expect_error(expect_warning(check_randomizr_arguments(N = "a")))
    
  expect_error(check_randomizr_arguments(N = -1))
  
  expect_error(check_randomizr_arguments(N = 100, prob = -1))
  expect_error(check_randomizr_arguments(N = 100, prob = 2))
  
  expect_error(check_randomizr_arguments(N = 100, m = -1))
  expect_error(check_randomizr_arguments(N = 100, m = 101))
  
  expect_error(check_randomizr_arguments(N = 100, m_each = c(-1, 2, 99)))
  expect_error(check_randomizr_arguments(N = 100, m_each = c(1, 2, 99)))
  
  expect_error(check_randomizr_arguments(N = 100, prob_each = c(-.5, 1.5)))
  expect_error(check_randomizr_arguments(N = 100, prob_each = c(.1,.2)))
})

