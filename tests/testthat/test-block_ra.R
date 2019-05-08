context("Block Random Assignments")

blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))
#blocks <- factor(blocks, levels = c("B", "A", "C"))
#blocks <- rep(1:3, times=c(50, 100, 200))


#debugonce(randomizr:::check_randomizr_arguments)
Z <- block_ra(blocks = blocks)
table(blocks, Z)

Z <- block_ra(blocks = blocks, block_m = c(20, 30, 40))
Z <- block_ra(blocks = blocks, m = 10)
expect_error(block_ra(blocks = blocks, m = 60))


Z <- block_ra(blocks = blocks, block_prob = c(.1, .2, .3))
Z <- block_ra(blocks = blocks, block_prob = c(0, .2, .3))
expect_error(block_ra(blocks = blocks, block_prob = c(.1, .2, .3, .4)))
expect_error(block_ra(blocks = blocks, block_prob = c(.1, .2, -.3)))
expect_error(block_ra(blocks = blocks, block_prob = c(.1, .2, 1.1)))

table(blocks, Z)



block_m_each <- rbind(c(25, 25),
                      c(50, 50),
                      c(100, 100))

Z <- block_ra(blocks = blocks, block_m_each = block_m_each)
table(blocks, Z)

block_m_each <- rbind(c(10, 40),
                      c(30, 70),
                      c(50, 150))

Z <- block_ra(
  blocks = blocks,
  block_m_each = block_m_each,
  conditions = c("control", "treatment")
)
table(blocks, Z)

# Multi-arm Designs
Z <- block_ra(blocks = blocks, num_arms = 2)
table(blocks, Z)
Z <- block_ra(blocks = blocks, num_arms = 3)
table(Z)
table(blocks, Z)


Z <- block_ra(blocks = blocks, num_arms = 4)
table(blocks, Z)
Z <- block_ra(blocks = blocks, num_arms = 5)
table(blocks, Z)
Z <- block_ra(blocks = blocks, num_arms = 6)
table(blocks, Z)

block_m_each <- rbind(c(10, 20, 20),
                      c(30, 50, 20),
                      c(50, 75, 75))
Z <- block_ra(blocks = blocks, block_m_each = block_m_each)
table(blocks, Z)

Z <- block_ra(
  blocks = blocks,
  block_m_each = block_m_each,
  conditions = c("control", "placebo", "treatment")
)
table(blocks, Z)

Z <- block_ra(blocks = blocks, prob_each = c(.1, .1, .8))
table(blocks, Z)
Z <- block_ra(blocks = blocks, prob_each = c(.31, .48, .21))
table(blocks, Z)
Z <- block_ra(blocks = blocks, prob_each = c(.213, .568, .219))
table(blocks, Z)


Z <- block_ra(blocks = blocks, prob = .5)
table(blocks, Z)
Z <- block_ra(blocks = blocks, prob = 1)
table(blocks, Z)
Z <- block_ra(blocks = blocks, prob = 0)
table(blocks, Z)
Z <- block_ra(blocks = blocks, prob = .33)
table(blocks, Z)


blocks <- rep(c("A", "B", "C"), times = c(51, 103, 207))

block_prob_each <- rbind(c(.3, .6, .1),
                         c(.2, .7, .1),
                         c(.1, .8, .1))

table(blocks, block_ra(blocks, block_prob_each = block_prob_each))



# Confirming Errors Correctly Thrown --------------------------------------


blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))

block_m_each <- rbind(c(25, 25),
                      c(50, 50),
                      c(100, 100))

block_prob_each <- rbind(c(.3, .6, .1),
                         c(.2, .7, .1),
                         c(.1, .8, .1))


block_ra(blocks = rep(c(T, F), c(5, 5)))



expect_error(
  block_ra(
    blocks = blocks,
    block_m_each = block_m_each,
    block_prob_each = block_prob_each
  )
)
expect_error(block_ra(
  blocks = blocks,
  block_m_each = block_m_each,
  prob_each = c(.2, .1, .7)
))
expect_error(block_ra(
  blocks = blocks,
  block_prob_each = block_prob_each,
  prob_each = c(.2, .1, .7)
))

expect_error(block_ra(
  blocks = blocks,
  num_arms = 2,
  block_prob_each = block_prob_each
))
expect_error(block_ra(
  blocks = blocks,
  num_arms = 2,
  prob_each = c(.2, .1, .7)
))
expect_error(block_ra(
  blocks = blocks,
  num_arms = 3,
  block_m_each = block_m_each
))

expect_error(block_ra(
  blocks = blocks,
  conditions = c("1", "2"),
  block_prob_each = block_prob_each
))
expect_error(block_ra(
  blocks = blocks,
  conditions = c("1", "2"),
  prob_each = c(.2, .1, .7)
))
expect_error(block_ra(
  blocks = blocks,
  conditions = c("1", "2", "3"),
  block_m_each = block_m_each
))

expect_error(block_ra(
  blocks = blocks,
  conditions = c("1", "2", "3"),
  num_arms = 2
))



cookie_type <- rep(c("sugar", "chip"), c(36, 36))
batch <-
  block_ra(
    blocks = cookie_type,
    block_m = c(18, 18),
    conditions = c("batch_1", "batch_2")
  )


test_that("Error", {
  blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))
  
  block_m_each <- rbind(c(25, 25),
                        c(50, 50),
                        c(100, 100))
  
  block_prob_each <- rbind(c(.3, .6, .1),
                           c(.2, .7, .1),
                           c(.1, .8, .2))
  
  expect_error(block_ra(blocks = blocks, block_prob_each = block_prob_each))
})


test_that("prob_unit and m_unit", {
  blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))
  
  Z <- block_ra(blocks = blocks, prob_unit = rep(c(.1, .2, .3), c(50, 100, 200)))
  expect_equal(table(blocks, Z), structure(c(45L, 80L, 140L, 5L, 20L, 60L), .Dim = 3:2, .Dimnames = list(
    blocks = c("A", "B", "C"), Z = c("0", "1")), class = "table"))
  
  expect_error(block_ra(blocks = blocks, prob_unit = rep(c(.1, .2, .3), c(200, 100, 50))),
               "In a block random assignment design, `prob_unit` must be the same for all units within the same block.")
  
  
  Z <- block_ra(blocks = blocks, m_unit = rep(c(20, 20, 25), c(50, 100, 200)))
  expect_equal(table(blocks, Z), structure(c(30L, 80L, 175L, 20L, 20L, 25L), .Dim = 3:2, .Dimnames = list(
    blocks = c("A", "B", "C"), Z = c("0", "1")), class = "table"))
  
  expect_error(block_ra(blocks = blocks, m_unit = rep(c(20, 20, 25), c(200, 100, 50))), 
               "In a block random assignment design, `m_unit` must be the same for all units within the same block.")
})



test_that("single unit in block (http://discuss.declaredesign.org/t/conditions-code/101)", {

  count = c(30, 27, 1, 2)
  block_count <- ceiling(count / 2)
  pre_rand_data <- data.frame(district = rep(c("cps","roe47","roe48","roe49"), times = count ), stringsAsFactors = FALSE)
  post_blk_rand <- within(pre_rand_data,{
    mfa_tx <- block_ra(blocks = district, block_m = block_count, conditions = c("k-2 mfa","3-5 mfa"))
    id_var <- 1:nrow(pre_rand_data)
  })
  
  expect_equal(class(post_blk_rand$mfa_tx), "factor")
  expect_equal(levels(post_blk_rand$mfa_tx), c("k-2 mfa", "3-5 mfa"))
  
})

