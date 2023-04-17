#' Test suite file with three worked examples and non-trivial data
#' Examples below show how a user can call mars()
#' and illustrates the functions created for the MARS object
#' Additional details can be found in the project documentation.

library(MASS)

#Example 0: Provided data set -------------------------------------------------

load("tests/testthat/testmc.RData")
load("tests/testthat/testmars.RData")
cat("\nExample 0: mars() function with provided dataset\n")
mars_out = mars(y~.,data=marstestdata,control=testmc)
anova(mars_out)
print(mars_out)
summary(mars_out)
plot(mars_out)

#Example 1: Boston city data from the MASS package ----------------------------

cat("Example 1: mars() function with Boston city data from the MASS package\n")

cat("\nExample 1: calling mars with default parameters\n")
#Using the mars() function with default control values
Boston.mars1 = mars(crim~age+nox+rm+dis+tax, Boston, control = mars.control())

#Running mars() function but changing the control argument
#Mmax is increased (2 -> 10)
#d is decreased (3 -> 2)
#These two will lead to more knots than Boston.mars1
cat("\nExample 1: calling mars with specified parameters (Mmax = 6, d = 2, trace=TRUE)\n")
Boston.mars2 = mars(crim~age+nox+rm+dis+tax, Boston,
                    control = mars.control(Mmax = 6, d = 2, trace = TRUE))

cat("\nExample 1: print\n")
#Method 1: print
print(Boston.mars1)
#2 B coefficients
print(Boston.mars2)
#5 B coefficients

#Method 2: summary
cat("\nExample 1: summary\n")
summary(Boston.mars1)
summary(Boston.mars2)

#Method 3: anova
cat("\nExample 1: print\n")
anova(Boston.mars1)
anova(Boston.mars2)

#Example 2: predict.mars with Iris data set -----------------------------------

cat("\nExample 2: predict.mars with Iris data set\n")

cat("\nExample 2: calling mars with default parameters\n")
# Applying predict.mars
Iris.Mars <-mars(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,
                 data=iris,control=mars.control())

cat("\nExample 2: predict\n")
prediction_iris_no_noise = predict(Iris.Mars)
print(prediction_iris_no_noise)
#Returns the "fitted.values" component of Iris.mars since data set is unchanged

#Altering the data set to see the how the prediction changes:
iris2 = iris
#Adding noise terms to the explanatory variables
set.seed(123)
iris2$Sepal.Width = round(iris$Sepal.Width + rnorm(150),1)
iris2$Petal.Length = round(iris$Petal.Length + rnorm(150),1)
iris2$Petal.Width = round(iris$Petal.Width + rnorm(150, sd = 0.1)+0.2,1)
head(iris2)

#New predicted values
cat("\nExample 2: prediction on dataset with noise\n")
prediction_iris_with_noise = predict(Iris.Mars,iris2)
print(prediction_iris_with_noise)

#Difference between the predicted values of the old and new data set
cat("\nExample 2: difference between the predicted values of the old and new data set\n")
print(prediction_iris_no_noise-prediction_iris_with_noise)

#Example 3: Plotting mars objects ---------------------------------------------

cat("\nExample 3: Plotting mars objects with Cars93 data from the MASS package\n")

#Using the Cars93 data set from the MASS package
cat("\nExample 3: calling mars with specified parameters (Mmax = 6, d = 3, trace = FALSE)\n")
Car.mars = mars(Rev.per.mile~EngineSize+Horsepower+RPM+as.factor(Origin)+
                  Fuel.tank.capacity+MPG.city+MPG.highway,
                data = Cars93, mars.control(Mmax = 6, d = 3, trace = FALSE))

#Plotting Car.Mars
#Cycle through the 4 plots that plot.mars produces
cat("\nExample 3: plot with default arguments\n")
plot(Car.mars)

#Alternatively, plots can be done individually
cat("\nExample 3: specify plot of interest (see documentation examples for more information)\n")

#Plot 1: Cumulative Distribution
cat("\nExample 3: Cumulative Distribution plot\n")
plot(Car.mars,1)

#Plot 2: Residuals vs. Fitted
cat("\nExample 3: Residuals vs. Fitted plot\n")
plot(Car.mars,2)

#Plot 3: Normal Q-Q
cat("\nExample 3: Normal Q-Q plot\n")
plot(Car.mars, 3)

#Plot 4: Scale-Location
cat("\nExample 3: Scale-Location plot\n")
plot(Car.mars, 4)

