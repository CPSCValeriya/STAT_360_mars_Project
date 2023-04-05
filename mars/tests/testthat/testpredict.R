load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testpredict.RData")
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testmars.RData")

test_that("predict.mars() returns the correct predictions on the same data used to fit the model", {expect_equal(predict.mars(testmars), testpredict)})

test_that("predict.mars() returns the correct predictions on new data", {
  expect_equal(predict.mars(testmars,newdata=marstestdata), testpredict)} )
