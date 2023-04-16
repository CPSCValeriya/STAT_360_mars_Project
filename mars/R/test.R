#' Test suite file with three worked examples and non-trivial data
#' Examples below show how a user can call mars()
#' and illustrates the functions created for the MARS object
#' Additional details can be found in the project documentation.

#Load data
load("tests/testthat/testmc.RData")
load("tests/testthat/testmars.RData")

#Test on mtcars dataset with default parameters
mtcars_mars = mars(mpg~., data=mtcars, control=mars.control())
anova(mtcars_mars)
print(mtcars_mars)
summary(mtcars_mars)
print(mtcars_mars)

#Test on provided dataset with default parameters
mars_out = mars(y~.,data=marstestdata,control=testmc)
anova(mars_out)
print(mars_out)
summary(mars_out)
print(mars_out)

#Test on provided dataset with specified arguments
mars_test = mars(y~.,data=marstestdata,control=mars.control(Mmax=6,d=3,trace=TRUE))
mars_test
anova(mars_test)
print(mars_test)
summary(mars_test)
print(mars_test)

