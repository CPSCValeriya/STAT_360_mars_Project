#' Title summary.mars
#'
#' @param mars a mars object, usually created by mars
#'
#' @return The function summary.mars computes and returns a list of summary statistics of the fitted linear model given in object,
#' using the components (list elements) "call" and "terms" from its argument
#'
#' @export
#'
#' @examples
summary.mars <- function(mars){

  class(mars) = class(mars)[2]; #get lm class
  print_basis(mars)
  print(summary(mars))
  class(mars) = c("mars",class(mars)) #reset classes

}
