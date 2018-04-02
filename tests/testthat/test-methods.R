context("Declaration Methods")

test_that("RA Methods",{
  d <- declare_ra(N=100)
  
  expect_error(d["foo"]<- 2)
  expect_error(d$foo <- 2)  
  expect_error(ra_function(NULL))
  
})

test_that("RS Methods",{
  d <- declare_rs(N=100)
  
  expect_error(d["foo"]<- 2)
  expect_error(d$foo <- 2)  
  
  expect_error(rs_function(NULL))
})