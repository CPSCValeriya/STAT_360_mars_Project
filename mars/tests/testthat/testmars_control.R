# testmars_control.R
library(mars)
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testmc.RData")
test_that("mars.control() returns the correct object", {
  expect_equal(mars.control(Mmax=10), testmc)
})

