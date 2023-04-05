load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testbwd_stepwise.RData")
test_that("bwd_stepwise() returns the correct object", {
  expect_equal(bwd_stepwise(testfwd,testmc), testbwd)
})
