load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testmc.RData")
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testmars.RData")
out = mars(y~.,data=marstestdata,control=testmc)
out
testmars

anova(out)
