#' Summary of a mars Object
#'
#' @description Outputs a detailed summary of the mars model object (includes the coefficients, basis functions, residuals, additional statistics and other)
#' @param mars a mars object, usually created by mars
#' @return The function summary.mars computes and returns a list of summary
#' statistics of the fitted linear model given in object,
#' using the components (list elements) "call" and "terms" from its argument
#' @import crayon
#' @export
#' @family methods
#' @examples
#' m <-mars(Ozone~Wind+Temp+Month,data=airquality,control=mars.control())
#' summary(m)
#' @importFrom stats summary.lm
summary.mars <- function(mars){

  print_basis(mars)
  cat(red$bold("\nPrinting mars object summary...\n"));
  summary.lm(mars)

}
