#' ANOVA for MARS Model Fits
#'
#' @param mars a mars object
#'
#' @export
#' @family methods
#'
#' @examples
#'
#' ## Analysing Ozone data
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
