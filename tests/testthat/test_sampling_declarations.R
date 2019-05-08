context("Sampling Declarations")

test_declaration <- function(declaration, esum, eprob){
  S <- draw_rs(declaration)
  prob <- obtain_inclusion_probabilities(declaration = declaration)

  if(!is.na(esum))expect_equal(sum(S), esum)
  if(!is.na(eprob))expect_true(all(prob == eprob))
  
  if(is.vector(declaration$clusters)){
    expect_true(all(colSums(table(S, declaration$clusters) != 0) == 1))
  }
  
}

# Complete Random Assignments ----------------------------------------------
test_that("Complete N = 100",{
  
  declaration <- declare_rs(N = 100)
  test_declaration(declaration, 50, .5)
  
})


test_that("Complete N = 101, prob = .34",{
  
  declaration <- declare_rs(N = 101, prob = .34)
  test_declaration(declaration, NA, .34)
})

test_that("simple + n fails",{
  
  expect_error(declare_rs(N = 101, n = 34, simple=TRUE))
})



test_that("N=100 n=50",{
  
  declaration <- declare_rs(N = 100, n = 50)
  test_declaration(declaration, 50, .5)
})


test_that("Simple N = 100",{
  
  declaration <- declare_rs(N = 100, simple = TRUE)
  test_declaration(declaration, NA, .5)
})


test_that("Simple N=100 prob=.4",{
  
  declaration <- declare_rs(N = 100, prob = .4, simple = TRUE)
  test_declaration(declaration, NA, .4)
})


test_that("strata",{
  
  strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))
  
  declaration <- declare_rs(strata = strata)
  test_declaration(declaration, 350/2, .5)
})

test_that("strata",{
  
  strata <- gl(3,100)
  
  declaration <- declare_rs(strata = strata)
  test_declaration(declaration, 150, .5)
})


test_that("clusters",{
  clusters <- rep(letters, times = 1:26)
  declaration <- declare_rs(clusters = clusters)
  test_declaration(declaration, NA, .5)
})

test_that("clusters, n=10",{
  clusters <- rep(letters[1:15], times = 1:15)
  declaration <- declare_rs(clusters = clusters, n = 10)
  test_declaration(declaration, NA, 2/3)
})



test_that("strata and clusters",{
  clusters <- rep(letters, times = 1:26)
  strata <- rep(NA, length(clusters))
  strata[clusters %in% letters[1:5]] <- "stratum_1"
  strata[clusters %in% letters[6:10]] <- "stratum_2"
  strata[clusters %in% letters[11:15]] <- "stratum_3"
  strata[clusters %in% letters[16:20]] <- "stratum_4"
  strata[clusters %in% letters[21:26]] <- "stratum_5"
  
  declaration <- declare_rs(clusters = clusters, strata = strata)
  test_declaration(declaration, NA, .5)
})


test_that("check errors",{
  
  expect_error(declare_rs(clusters=c(1,1,1,1), strata = c(1,2,1,2)))
  expect_error(declare_rs(N=9, strata = c(1,1,2,2)))
  expect_error(declare_rs(prob=.2))
  expect_error(declare_rs(N=4, prob=.2, n=.3))
  
})

test_that("check deprecations",{
  # TODO remove when below are also removed, this is just for test coverage
  d <- declare_rs(N=10, n=4)  
  expect_warning(d$rs_function())
  expect_warning(d$rs_type)
  expect_warning(d$cleaned_arguments)
})

test_that("draw_rs auto-declare",{
  expect_equal(draw_rs(N=1, prob=1), 1)
  expect_error(draw_rs(sleep))
  
})


test_that("obtain_inclusion_probabilities auto-declare",{
  expect_equal(obtain_inclusion_probabilities(N = 1), .5)
  expect_error(obtain_inclusion_probabilities(sleep))
  
})


test_that("print and summary", {
  d <- declare_rs(N = 10, n = 4)
  expect_output(print(d))
  expect_output(summary(d))
})


test_that("_each",{
  
  strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))
  d <- declare_rs(strata = strata, n_unit = rep(c(20, 20, 25), c(50, 100, 200)))
  
  expect_equal(table(strata, draw_rs(d)), 
               structure(c(30L, 80L, 175L, 20L, 20L, 25L), .Dim = 3:2, .Dimnames = list(
                 strata = c("A", "B", "C"), c("0", "1")), class = "table"))
  
  expect_error(
    declare_rs(strata = strata, n_unit = rep(c(20, 20, 25), c(200, 100, 50))),
    "In a stratified random sampling design, `n_unit` must be the same for all units within the same stratum."
  )
})
