context("Tricky Examples")

test_that("B <- rep(2:4,9)", {
  B <- rep(2:4,9)
  
  golden <-   structure(c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), .Dim = c(3L, 
                        3L), .Dimnames = structure(list(B = c("2", "3", "4"), c("T1", 
                        "T2", "T3")), .Names = c("B", "draw")), class = "table")
  
  
  # Checks out
  draw <- block_ra(blocks = B, prob_each = rep(1/3,3))
  expect_identical(
    table(B, draw),
    golden
  )

  draw <- block_ra(blocks = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33)))
  expect_identical(
    table(B, draw),
    golden
  )
  
  
  # randomizr doesn't rescale
  expect_error(table(B, block_ra(blocks = B, prob_each = c(.33,.33,.33))))

})

# Works
test_that("ABAD",{
  B <- c("A", "B", "A", "D")
  draw <- block_ra(blocks = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33)))
  
  expect_true(all(table(B, draw) %in% 0:2))
})

test_that("ABD",{
  B <- c("A", "B", "D")
  draw <- block_ra(blocks = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33)))
  expect_true(all(table(B, draw) %in% 0:1))
  
  B <- c(B,B)
  draw <- block_ra(blocks= B, prob_each=c(.43,.33,.33)/sum(c(.43,.33,.33)))
  expect_true(all(table(B, draw) %in% 0:2))
})


test_that("B=12121",{
  B = c(1,2,1,2,1)
  draw = block_ra(blocks = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33)))
  expect_equivalent(
    as.numeric(table(B, draw)[1,]),
    c(1,1,1)
  )
})

# Complete random assignment for factorial
test_that("Complete N=16",{
  expect_equivalent(
    as.numeric(table(complete_ra(16))),
    c(8,8)
  )
})

test_that("Complete N=16 p=.25",{
  expect_equivalent(
    as.numeric(table(complete_ra(16, prob=.25))),
    c(12,4)
  )
})





test_that("Complete 16 ABCD",{
  # Complete random assignment into 4 categories eg for factorial
  draw <- complete_ra(16, prob_each = rep(.25, 4), conditions = c("T00", "T01", "T10", "T11"))
  expect_true(all(table(draw) == 4))
})

# Block examples

test_that("B=AABB",{
  B <- c("A","A","B","B")
  expect_true(all(table(B, block_ra(blocks = B)) == 1))
})


test_that("B=1122 ABC",{

  B <- c(1,1,2,2)
  draw <- block_ra(blocks =B, prob_each=c(.21,.29,.5))
  expect_true(all(table(B, draw) %in% 0:1))
})


test_that("B=111222",{

  # Global balance even if within block balance not possible
  B <- c(1,1,1,2,2,2)
  draw <- block_ra(blocks = B, prob = .5)
  t <- table(B, draw)
  
  expect_equivalent(as.numeric(sort(t[1,])), 1:2)
  expect_equivalent(as.numeric(sort(t[1,])), 1:2)
})


test_that("B=1112222",{
  B <- c(1,1,1,2,2,2,2)
  draw <- block_ra(blocks = B, prob = .5)
  t <- table(B, draw)
  
  expect_equivalent(as.numeric(sort(t[1,])), 1:2)
  expect_equivalent(as.numeric(t[2,]), c(2,2))
  
})


test_that("B=111222222",{
  B <- c(1,1,1,2,2,2,2,2,2)
  draw <- block_ra(blocks = B, prob_each =c(1/3,1/3,1/3))
  golden <- structure(c(1L, 2L, 1L, 2L, 1L, 2L), .Dim = 2:3, .Dimnames = structure(list(
    B = c("1", "2"), draw = c("T1", "T2", "T3")), .Names = c("B", 
    "draw")), class = "table")
  
  expect_identical(
    table(B, draw),
    golden
  )
})


test_that("B=111222222344 ABCD",{
  B <- c(1,1,1,2,2,2,2,2,2, 3, 4, 4)
  draw <- block_ra(blocks = B, prob_each =c(1/6,1/6,1/6, 1/2), 
                   conditions = c("A", "B", "C", "D"))
  
  expect_true(all(table(B, draw)[2,] >= 1))

})


test_that("balancing with block_prob_each",{
  # Bonus trick to show 
  
  blocks <- rep(c("A", "B","C"), times=c(51, 103, 207))
  
  block_prob_each <- rbind(c(.3, .6, .1),
                           c(.2, .7, .1),
                           c(.1, .8, .1))
  
  
  draw <- block_ra(blocks, block_prob_each = block_prob_each)
  
  golden <- structure(c(15L, 21L, 20L, 31L, 72L, 165L, 5L, 10L, 22L), .Dim = c(3L, 
                      3L), .Dimnames = structure(list(blocks = c("A", "B", "C"), draw = c("T1", 
                      "T2", "T3")), .Names = c("blocks", "draw")), class = "table")
  
  expect_true(all (
      table(blocks, draw) - golden %in% -1:1
  ))
})



