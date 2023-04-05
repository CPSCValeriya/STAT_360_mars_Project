print.mars <- function(mars){

  class(mars) = class(mars)[2]; #get lm class
  print_basis(mars);
  cat("\n");
  print_product(mars);
  cat("\n");
  cat(cyan$bold("Printing mars object call...\n\n"));
  print(mars$call)
  cat("\n")
  cat(cyan$bold("Printing mars object coefficients...\n\n"));
  print(mars$coefficients)
  cat("\n")
  class(mars) = c("mars",class(mars)) #reset classes

}
