context("restrictedparts.c")

test_that("growing the buffer works",{
  
  expect_equal(restrictedparts(32, 4)[[351]], c(8,8,8,8))
})