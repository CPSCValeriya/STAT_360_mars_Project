#' ANOVA for MARS Model Fits
#'
#' Implementation of ANOVA for mars object
#'
#' @title ANOVA for MARS Model Fits
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
