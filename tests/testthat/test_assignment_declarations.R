
context("Declarations: Complete Random Assignments")

test_declaration <- function(declaration, esum, eprob, conditions){
  Z <- conduct_ra(declaration)
  
  if(!is.null(declaration$N)) expect_length(Z, declaration$N)
  
  prob <- obtain_condition_probabilities(declaration = declaration, assignment = conditions)
  
  expect_true(is.numeric(prob))
  
  if(!is.na(esum))expect_equal(sum(Z), esum)
  if(!is.na(eprob))expect_true(all(prob == eprob))
  
  if(is.vector(declaration$clusters)){
    expect_true(all(colSums(table(Z, declaration$clusters) != 0) == 1))
  }
  
}

# Complete Random Assignments ----------------------------------------------
test_that("default",{
  declaration <- declare_ra(N=100)
  test_declaration(declaration, 50, .5, 0:1)
})

test_that("N=101, prob=.34",{
  declaration <- declare_ra(N=100, prob = .34)
  test_declaration(declaration, 34, .34, 1)
})

test_that("N=100, m=50",{
  declaration <- declare_ra(N=100, m=50)
  test_declaration(declaration, 50, .5, 0:1)
})

test_that("N=100, m_each",{
  declaration <- declare_ra(N=100, m_each = c(30, 70), 
                            conditions = c("control", "treatment"))
  test_declaration(declaration, NA, .3, "control")
  test_declaration(declaration, NA, .7, "treatment")
})


# Multi-arm Designs
test_that("m_each=c(30, 30, 40)",{
  declaration <- declare_ra(N=100, m_each=c(30, 30, 40))
  test_declaration(declaration, NA, .3, "T1")
  test_declaration(declaration, NA, .3, "T2")
  test_declaration(declaration, NA, .4, "T3")  
})

test_that("named conditions m_Each",{
  declaration <- declare_ra(N=100, m_each=c(30, 30, 40), 
                          conditions=c("control", "placebo", "treatment"))
  test_declaration(declaration, NA, .3, "control")
  test_declaration(declaration, NA, .3, "placebo")
  test_declaration(declaration, NA, .4, "treatment")
})

test_that("names",{
  declaration <- declare_ra(N=100, conditions=c("control", "placebo", "treatment"))
  test_declaration(declaration, NA, 1/3, "control")
  test_declaration(declaration, NA, 1/3, "placebo")
  test_declaration(declaration, NA, 1/3, "treatment")
})

test_that("num_arms",{
  declaration <- declare_ra(N=100, num_arms=3)
  test_declaration(declaration, NA, 1/3, "T1")
  test_declaration(declaration, NA, 1/3, "T2")
  test_declaration(declaration, NA, 1/3, "T3")  
})


test_that("simple + m fails",{
  
  expect_error(declare_ra(N = 101, m = 34, simple=TRUE))
})


context("Declarations: Simple Random Assignments")

  
# Simple Random Assignmetn ------------------------------------------------
test_that("simple",{
  declaration <- declare_ra(N=100, simple = TRUE)
  test_declaration(declaration, NA, .5, 0)  
})


test_that("simple p = .4",{
  declaration <- declare_ra(N=100, prob = .4, simple = TRUE)
  test_declaration(declaration, NA, .4, 1)  
})

test_that("simple named prob each",{
  declaration <- declare_ra(N=100, prob_each = c(0.3, 0.7), 
                            conditions = c("control", "treatment"), simple=TRUE)
  test_declaration(declaration, NA, .3, "control")
  test_declaration(declaration, NA, .7, "treatment")
})

test_that("simple num_arms = 3",{
  declaration <- declare_ra(N=100, num_arms=3, simple=TRUE)
  test_declaration(declaration, NA, 1/3, "T1")
  test_declaration(declaration, NA, 1/3, "T2")
  test_declaration(declaration, NA, 1/3, "T3")  
})

test_that("simple 3 armed prob each",{
  declaration <- declare_ra(N=100, prob_each=c(0.3, 0.3, 0.4), simple=TRUE)
  test_declaration(declaration, NA, .3, "T1")
  test_declaration(declaration, NA, .3, "T2")
  test_declaration(declaration, NA, .4, "T3")
})

test_that("simple 3 arm prob each named",{
  
  declaration <- declare_ra(N=100, prob_each=c(0.3, 0.3, 0.4), 
                            conditions=c("control", "placebo", "treatment"), simple=TRUE)
  test_declaration(declaration, NA, .3, "control")
  test_declaration(declaration, NA, .3, "placebo")
  test_declaration(declaration, NA, .4, "treatment")
})

test_that("simple names 3 armed",{
  declaration <- declare_ra(N=100, conditions=c("control", "placebo", "treatment"), simple=TRUE)
  test_declaration(declaration, NA, 1/3, "control")
  test_declaration(declaration, NA, 1/3, "placebo")
  test_declaration(declaration, NA, 1/3, "treatment")
})


context("Declarations: Block Random Assignments")
# Blocked Designs ---------------------------------------------------------


test_that("Blocks default",{
  blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
  declaration <- declare_ra(blocks=blocks)
  test_declaration(declaration, 175, .5, 1)
})

test_that("Blocks default w/ factor",{
  blocks <- gl(3,100)
  declaration <- declare_ra(blocks=blocks)
  test_declaration(declaration, 150, .5, 1)
})

test_that("blocks m_each",{
  blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
  block_m_each <- rbind(c(25, 25),
                   c(50, 50),
                   c(100, 100))
  declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each)
  test_declaration(declaration, 175, .5, 1)
})

test_that("block_m_each different",{
  blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
  block_m_each <- rbind(c(10, 40),
                   c(30, 70),
                   c(50, 150))
  declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each)
  test_declaration(declaration, 260, NA, "A")
  expect_equal(
    obtain_condition_probabilities(declaration = declaration, assignment = 1)[c(1, 88, 175)],
    c(.8, .7, .75)
  )
})

test_that("block_m_eahc named",{
  blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
  block_m_each <- rbind(c(10, 40),
                        c(30, 70),
                        c(50, 150))
  
  declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each, 
                            conditions=c("control", "treatment"))

  expect_equal(
    obtain_condition_probabilities(declaration = declaration, assignment = "treatment")[c(1, 88, 175)],
    c(.8, .7, .75)
  )
})


# Multi-arm Designs
test_that("Three arm block_m_each",{
  blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
  block_m_each <- rbind(c(10, 20, 20),
                 c(30, 50, 20),
                 c(50, 75, 75))
  declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each)
  test_declaration(declaration, NA, NA, "T1")
  expect_equal(
    obtain_condition_probabilities(declaration = declaration, assignment = "T1")[c(1, 88, 175)],
    c(.2, .3, .25)
  )

})

test_that("blocks num_arms = 3 ",{
  blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
  
  declaration <- declare_ra(blocks=blocks, num_arms=3)
  test_declaration(declaration, NA, 1/3, "T1")
  
  expect_true(all(table(conduct_ra(declaration), blocks) > 10))
})

test_that("block_m_each named",{
  blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
  block_m_each <- rbind(c(10, 20, 20),
                        c(30, 50, 20),
                        c(50, 75, 75))
  
  declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each, 
                            conditions=c("control", "placebo", "treatment"))
  test_declaration(declaration, NA, NA, "treatment")
})


test_that("blocks prob_each",{
  blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
  
  declaration <- declare_ra(blocks=blocks, prob_each=c(.1, .1, .8))
  test_declaration(declaration, NA, .1, "T1")
})

context("Declarations: Cluster Random Assignments")

# Cluster Random Assignments ----------------------------------------------

clusters <- rep(letters, times=1:26)

# Two Group Designs
test_that("Two Group clusters",{
  declaration <- declare_ra(clusters=clusters)
  test_declaration(declaration, NA, .5, 1)
})


test_that("clusters, m=13",{
  declaration <- declare_ra(clusters=clusters, m=13)
  test_declaration(declaration, NA, .5, 1)
})

test_that("cluster m_each",{
  declaration <- declare_ra(clusters=clusters, m_each = c(10, 16), 
                  conditions = c("control", "treatment"))
  test_declaration(declaration, NA, 16/26, "treatment")
})

test_that("Multi-arm Designs",{
  declaration <- declare_ra(clusters=clusters, num_arms=3)
  test_declaration(declaration, NA, 1/3, "T1")
  
})

test_that("clusters m_each three arms",{
  declaration <- declare_ra(clusters=clusters, m_each=c(7, 7, 12))
  test_declaration(declaration, NA, 7/26, "T1")
})

test_that("clusters m_each three arms named",{
  declaration <- declare_ra(clusters=clusters, m_each=c(7, 7, 12), 
                  conditions=c("control", "placebo", "treatment"))
  test_declaration(declaration, NA, 7/26, "placebo")
})

test_that("clusters three conditons",{
  declaration <- declare_ra(clusters=clusters, 
                  conditions=c("control", "placebo", "treatment"))
  test_declaration(declaration, NA, 1/3, "placebo")
})

test_that("cluster prob_each three arm",{
  declaration <- declare_ra(clusters=clusters, prob_each = c(.1, .2, .7))
  test_declaration(declaration, NA, .2, "T2")
})

context("Declarations: Blocked and Cluster Random Assignments")
# Blocked and cluster -----------------------------------------------------

clusters <- rep(letters, times=1:26)
blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:5]] <- "block_1"
blocks[clusters %in% letters[6:10]] <- "block_2"
blocks[clusters %in% letters[11:15]] <- "block_3"
blocks[clusters %in% letters[16:20]] <- "block_4"
blocks[clusters %in% letters[21:26]] <- "block_5"


test_that("blocks, clusters",{
  declaration <- declare_ra(clusters = clusters, blocks = blocks)
  test_declaration(declaration, NA, .5, 1)
})

test_that("blocks, clusters numarm=3",{
  declaration <- declare_ra(clusters = clusters, blocks = blocks, num_arms = 3)
  test_declaration(declaration, NA, 1/3, "T1")
})

test_that("blocks clusters probeach three arm",{
  declaration <- declare_ra(clusters = clusters, blocks = blocks, prob_each = c(.2, .5, .3))
  test_declaration(declaration, NA, .2, "T1")
})

test_that("block clusters block_m_each",{
    
  block_m_each <- rbind(c(2, 3),
                   c(1, 4),
                   c(3, 2),
                   c(2, 3),
                   c(5, 1))
  
  declaration <- declare_ra(clusters = clusters, blocks = blocks, block_m_each = block_m_each)
  test_declaration(declaration, NA, NA, "T1")
  expect_equal(
    obtain_condition_probabilities(declaration = declaration, assignment = 1)[c(1,23,56,122)],
    c(.6, .8, .4, .6)
  )
})

test_that("big permutation matrix",{
  
  pm <- obtain_permutation_matrix(declare_ra(N=12))
  expect_equal( ncol(unique(pm, MARGIN=2))   , ncol(pm))
})

test_that("check errors",{
  
  expect_error(declare_ra(clusters=c(1,1,1,1), blocks = c(1,2,1,2)))
  expect_error(declare_ra(N=9, blocks = c(1,1,2,2)))
  expect_error(declare_ra(prob=.2))
  expect_error(declare_ra(N=4, prob=.2, prob_each=.3))
  
})

test_that("check deprecations",{
  # TODO remove when below are also removed, this is just for test coverage
  d <- declare_ra(N=10, n=4)  
  expect_warning(d$ra_function())
  expect_warning(d$ra_type)
  expect_warning(d$cleaned_arguments)
})

test_that("conduct_ra auto-declare",{
  expect_equal(conduct_ra(N=1, prob=1), 1)
  expect_error(conduct_ra(sleep))
  
})


test_that("obtain_condition_probabilities auto-declare",{
  expect_equal(obtain_condition_probabilities(assignment = 1), .5)
  expect_error(obtain_condition_probabilities(sleep))
  
})

