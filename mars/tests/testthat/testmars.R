library(mars)
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testmars.RData")
test_that("mars() returns the correct object", {
  expect_equal(mars(y~.,data=marstestdata,control=testmc),
               testmars, ignore_attr=TRUE)
})

