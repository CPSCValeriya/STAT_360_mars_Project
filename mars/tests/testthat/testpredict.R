library(mars)
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testpredict.RData")
load("C:/Users/Valerie/Downloads/STAT 360/mars_project/STAT_360_mars_Project/mars/tests/testthat/testmars.RData")
test_that("predict.mars() returns the correct predictions on the same data used to fit the model", {
  expect_equal(predict.mars(testmars), testpredict)} )
test_that("predict.mars() returns the correct predictions on new data", {
  expect_equal(predict.mars(testmars,newdata=marstestdata), testpredict)} )


# --------------------------------------------------

# b=testfwd$Bfuncs
# b
#
# num_Bfuncs = length(b)
# B <- data.frame(matrix(1,nrow=nrow(testfwd$B),ncol=num_Bfuncs))
# for(i in 2:num_Bfuncs){
#   num_products = nrow(data.frame(b[i]))
#   for(j in 1:num_products){
#     #Retrieve hinge function information from relevant Bfuncs
#     curr_hinge = b[[i]][j,]
#     v = curr_hinge[["v"]]
#     s = curr_hinge[["s"]]
#     t = curr_hinge[["t"]]
#     B[,i] = B[,i] * h(x[,v],s,t)
#   }
# }
#
# predict = 0;
# for(i in 1:length(beta)){
#   predict = predict + beta[[i]]*B[[i]]
# }
