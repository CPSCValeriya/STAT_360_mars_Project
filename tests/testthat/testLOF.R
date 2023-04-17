load("testLOF.RData")
load("testmc.RData")
load("testfwd_stepwise.RData")
data = data.frame(y=testfwd$y,testfwd$B)
test_that("testing LOF", {
  expect_equal(LOF(y~.-1, data, testmc), testLOF)} )
