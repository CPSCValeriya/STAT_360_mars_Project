load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testmc.RData")
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testmars.RData")

#Test on provided dataset
out = mars(y~.,data=marstestdata,control=testmc)
out
testmars

anova(out)
print(out)
summary(out)
print(out)

#Test on mtcars dataset
mtmars = mars(mpg~., data=mtcars, control=mars.control())
mtmars
plot(mtmars)

mtmars = mars(mpg~., data=mtcars)
mtmars

#Test with earth
library(earth)
mtearth <- earth(mpg~., data=mtcars)
summary(mtearth, style = "pmax")
summary(mtearth)
