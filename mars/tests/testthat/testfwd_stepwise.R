# testfwd_stepwise.R
library(mars)
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testfwd_stepwise.RData")
testthat::test_that("fwd_stepwise() returns the correct object", {
  expect_equal(fwd_stepwise(testy,testx,testmc), testfwd)
})
