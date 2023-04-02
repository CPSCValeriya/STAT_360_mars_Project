library(mars)
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testbwd_stepwise.RData")
test_that("bwd_stepwise() returns the correct object", {
  expect_equal(bwd_stepwise(testfwd,testmc), testbwd)
})

bwd_stepwise(testfwd,testmc)

#jstar1:  3 4 7 8 9 11
#jstar2:  1 3 4 7 8 9 11 (final jstar)

#  expected: B0 B1 B3 B6 B7 B8 B10
#  actual:  B0 *B2* B3 B6 B7 B8 B10
