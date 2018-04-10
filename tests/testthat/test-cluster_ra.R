
context("Cluster Random Assignments")
clusters <- rep(letters[1:15], times=1:15)

expect_only_one_per_cluster <- function(Z, w=1){
  expect_true(all(colSums(table(Z, clusters) != 0) == w))
}

test_that("Two Group Designs",{
  
  expect_only_one_per_cluster( cluster_ra(clusters=clusters))
  
  
  expect_only_one_per_cluster( cluster_ra(clusters=clusters, m=13) )

  
  expect_only_one_per_cluster( 
    cluster_ra(clusters=clusters, m_each = c(10, 5), 
                  conditions = c("control", "treatment"))
  )

  expect_only_one_per_cluster( cluster_ra(clusters=clusters, prob = .5) )
  expect_true(  all( cluster_ra(clusters=clusters, prob = 0) == 0 ))
  expect_only_one_per_cluster( 
    cluster_ra(clusters=clusters, prob = 1)
  )
})
test_that("Multi-arm Designs",{
  
# 
  expect_only_one_per_cluster( 
    cluster_ra(clusters=clusters, num_arms=3) 
  )

  expect_only_one_per_cluster( 
    cluster_ra(clusters=clusters, m_each=c(3, 5, 7))
  )

  expect_only_one_per_cluster( 
    cluster_ra(clusters=clusters, m_each=c(4, 4, 7), 
                conditions=c("control", "placebo", "treatment"))
  )

  expect_only_one_per_cluster( 
    cluster_ra(clusters=clusters, 
                conditions=c("control", "placebo", "treatment"))
  )

  expect_only_one_per_cluster( 
    cluster_ra(clusters=clusters, prob_each = c(.1, .2, .7))
  )
})


test_that("Two Group Designs",{
  expect_error(cluster_ra(clusters=gl(10,2), m=3, simple=TRUE))
  expect_error(cluster_ra(clusters=gl(10,2), m_each=c(3,7), simple=TRUE))
})


test_that("simple assignment",{
  clusters <- gl(100, 2)
  expect_equal(sum(cluster_ra(clusters, prob = 1, simple=TRUE)), 200)
})