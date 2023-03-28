#' Multivariate Adaptive Regression Splines (MARS)
#'
#' Fit Friedman's Multivariate Adaptive Regression Splines (MARS) model.
#'
#' @param formula an R formula
#' @param data a data frame containing the data
# ....
# .....

mars <- function(formula,data,control=mars.control()) {
  cc <- match.call() # save the call
  mf <- model.frame(formula,data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1,drop=FALSE]
  x_names <- colnames(x)
  control <- validate_mars.control(control)
  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B)) # notice -1 added
  out <- c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,
                x_names=x_names),fit)
  class(out) <- c("mars",class(fit))
  out
}


fwd_stepwise <- function(y,x,control=mars.control()){
}

init_B <- function(N,Mmax) {

}

bwd_stepwise <- function(fwd,control) {
}

LOF <- function(form,data,control) {
  # update this LOF to GCV
}

h <- function(x,s,t) {
  ...?pmax()
  # if x>t, s=+1, this return max(0,x-t)
  # if x<t, s=-1, this return max(0,t-x)
}

split_points <- function(xv,Bm) {
  #...
}

#------------------------------------------------------------------------
# constructor, validator and helper for class mars.control
#------------------------------------------------------------------------
#
new_mars.control <- function(control) {
}

validate_mars.control <- function(control) {

}


#' Constructor for `mars.control` objects
#'
#' This function constructs a `mars.control` object that specifies
#' parameters used in the model fitting procedure.
#'
#' @param Mmax Maximum number of basis functions. Should be an even integer. Default value is 2.
# .....
# ...

mars.control <- function(Mmax=2,d=3,trace=FALSE) {
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}
