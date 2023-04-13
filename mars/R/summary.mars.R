#' Title summary.mars
#'
#' @param mars a mars object, usually created by mars
#'
#' @return The function summary.mars computes and returns a list of summary statistics of the fitted linear model given in object,
#' using the components (list elements) "call" and "terms" from its argument
#'
#' @export
#' @family methods
#'
#' @examples
#' @importFrom stats summary.lm
summary.mars <- function(mars){

  print_basis(mars)
  cat(red$bold("\nPrinting mars object summary...\n"));
  summary.lm(mars)

}
