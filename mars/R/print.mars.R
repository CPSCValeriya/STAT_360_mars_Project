#' Print values for a mars Object
#'
#' @param mars a mars object, usually created by mars
#'
#' @export
#' @family methods
#'
#' @examples
#' ## Print output for fitting a mars to the mtcars data
#' mtmars = mars(mpg~., data=mtcars, control=mars.control())
#' print(mtmars)
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
