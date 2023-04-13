#' ANOVA for MARS Model Fits
#'
#' @param mars a mars object
#'
#' @return
#' @export
#'
#' @examples
anova.mars <- function(mars){

  class(mars) = class(mars)[2]; #get lm class
  print_basis(mars);
  cat("\n");
  cat(red$bold("Printing mars object ANOVA results...\n\n"));
  print(anova(mars))
  class(mars) = c("mars",class(mars)) #reset classes

}
