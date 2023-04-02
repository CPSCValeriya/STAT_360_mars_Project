load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testLOF.RData")
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testmc.RData")
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testfwd_stepwise.RData")
data = data.frame(y=testfwd$y,testfwd$B)
test_that("testing LOF", {
  expect_equal(LOF(y~.-1, data, testmc), testLOF)} )
