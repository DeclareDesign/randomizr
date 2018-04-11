context("Declaration Methods")

test_that("RA Methods",{
  d <- declare_ra(N=100)
  
  expect_warning(d[["foo"]] <- 2)
  expect_warning(d$foo <- 2)  
  expect_error(ra_function(NULL))
  
  
  ### Testing pretty printer
  expect_output(print(d), "Complete random assignment")
  expect_output(print(d), "Number of units: 100")
  expect_output(print(d), "Number of treatment arms: 2")
  expect_output(print(d), "0 and 1")
  expect_output(print(d), "are constant across units")
  
    
  
})

test_that("RA block clust pretty printer",{
  d <- declare_ra(blocks =c(1,1,1,2,2,2), clusters=c(1,1,2,3,3,4), block_prob = c(.3,.7))
  
  ### Testing pretty printer
  expect_output(print(d), "Blocked and clustered random assignment")
  expect_output(print(d), "Number of units: 6")
  expect_output(print(d), "Number of blocks: 2")
  expect_output(print(d), "Number of clusters: 4")
  expect_output(print(d), "NOT constant across units")
  
})


test_that("RS Methods",{
  d <- declare_rs(N=100)
  
  expect_warning(d[["foo"]]<- 2)
  expect_warning(d$foo <- 2)  
  
  expect_error(rs_function(NULL))
  
  
  ### Testing pretty printer
  expect_output(print(d), "Complete random sampling")
  expect_output(print(d), "Number of units: 100")
  expect_output(print(d), "are constant across units")
  
})

test_that("RS strata clust pretty printer",{
  d <- declare_rs(strata=c(1,1,1,2,2,2), clusters=c(1,1,2,3,3,4), strata_prob = c(.3,.7))
  
  ### Testing pretty printer
  expect_output(print(d), "Stratified and clustered random sampling")
  expect_output(print(d), "Number of units: 6")
  expect_output(print(d), "Number of strata: 2")
  expect_output(print(d), "Number of clusters: 4")
  expect_output(print(d), "NOT constant across units")
  
})