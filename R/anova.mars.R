#' ANOVA for mars Objects
#'
#' @title ANOVA for mars objects
#' @description Outputs the Analysis of Variance table for the passed in mars object, as well as additional information on the passed in object.
#' @param mars a mars object
#' @import crayon
#' @export
#' @family methods
#' @examples
#' ## Analyzing Ozone data with mars function
#' m <-mars(Ozone~Wind+Temp+Month,data=airquality,control=mars.control())
#' anova(m)
anova.mars <- function(mars){

  class(mars) = class(mars)[2]; #get lm class
  print_basis(mars);
  cat("\n");
  cat(red$bold("Printing mars object ANOVA results...\n\n"));
  print(anova(mars))
  class(mars) = c("mars",class(mars)) #reset classes

}
