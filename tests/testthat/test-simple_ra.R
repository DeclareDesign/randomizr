
context("Simple Random Assignments")

expect_u <- function(Z, lvl){
  expect_true(all(Z %in% lvl))
}

test_that("Two Group Designs", {
  expect_u(
    simple_ra(N=100),
    0:1
  )

  
  expect_u(
    simple_ra(N=100, prob=0.5),
    0:1
  )
  expect_u(
    simple_ra(N=100, prob_each = c(0.3, 0.7), 
                 conditions = c("control", "treatment")),
    c("control", "treatment")
  )

})
test_that("Multi-arm Designs", {
  expect_u(
    simple_ra(N=100, num_arms=3),
    c("T1", "T2", "T3")
  )
  
  expect_u(
    simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4)),
    c("T1", "T2", "T3")
  )
  
  expect_u(
    simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4), 
                 conditions=c("control", "placebo", "treatment")),
    c("control", "placebo", "treatment")
  )
  
  expect_u(
    simple_ra(N=100, conditions=c("control", "placebo", "treatment")),
    c("control", "placebo", "treatment")
  )
  
  
  
})

test_that("Special Cases", {

  expect_u(simple_ra(N=1, prob = 0), 0)
  expect_u(simple_ra(N=1, prob = 1), 1)
  
  expect_u(simple_ra(N=3, prob = 0), 0)
  expect_u(simple_ra(N=3, prob = 1), 1)

  expect_u(simple_ra(N=1, prob_each = c(0, 1)), 1)
  expect_u(simple_ra(N=1, prob_each = c(0, 0, 1)), "T3")
  expect_u(simple_ra(N=3, prob_each = c(0, 1)), 1)
  expect_u(simple_ra(N=4, prob_each = c(0, 0, 1)), "T3")

})


test_that("Weighted", {
  
  p1 <- simple_ra_probabilities(N=4, prob_each=c(.1, .9))
  p2 <- simple_ra_probabilities(N=4, prob=c(.9))
  
  expect_equal(p1, p2)
  
  p3 <- simple_ra_probabilities(N=4, prob_each=cbind(.1, .1, .8))
  p4 <- simple_ra_probabilities(N=4, prob_each=c(.1, .1, .8))
  
  expect_equal(p3, p4)

  p5 <- simple_ra_probabilities(N=10, prob=1:10/10)
  p6 <- simple_ra_probabilities(N=10, prob_each=cbind(9:0/10, 1:10/10))
  
  
  expect_equal(p5, p6)
  

  expect_true(all(replicate(100, simple_ra(N=10, prob=1:10/10)[10] == 1)))
  
  
})



