summary.mars <- function(mars){

  class(mars) = class(mars)[2]; #get lm class
  print_basis(mars)
  print(summary(mars))
  class(mars) = c("mars",class(mars)) #reset classes

}
