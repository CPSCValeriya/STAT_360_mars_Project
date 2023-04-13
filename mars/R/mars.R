#' Multivariate Adaptive Regression Splines (MARS)
#'
#' Fit Friedman's Multivariate Adaptive Regression Splines (MARS) model.
#'
#' @param formula an R formula
#' @param data a data frame containing the a response variable and predictors
#' @param control a mars.control object created using mars.control()
#'
#' @return An S3 model of class "mars"
#'
#' @references
#' Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1),
#'  1–67. https://doi.org/10.1214/aos/1176347963
#'
#'@seealso
#'\describe{
#'\item{anova.mars}{}
#'\item{plot.mars}{}
#'\item{predict.mars}{}
#'\item{print.mars}{}
#'\item{summary.mars}{}
#'}
#'
#'@examples
#'## Analyzing flower value data found in the iris dataset
#'
#'mars.mod <- mars(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=iris,control=mars.control())
#'print(mars.mod)

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

#' Forward Stepwise
#'
#' Implementing algorithm 2 from the page 17 of the Friedman Paper
#' @param y vector of response values
#' @param x dataset of predictor variables
#' @param control mars.control object created by mars.control
#'
#' @return A list with elements
#' \describe{
#' \item{y The same vector of response values given as input}{}
#' \item{B }{}
#' \item{Bfuncs }{}
#' }
#' @export
#'
#' @references
#' Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1),
#'  1–67. https://doi.org/10.1214/aos/1176347963
#'
fwd_stepwise <- function(y,x,control=mars.control()){

  Mmax = control$Mmax;

  N <- length(y)
  n <- ncol(x)
  B <- init_B(N,Mmax)
  Bfuncs = vector(mode = "list", length = Mmax + 1)
  splits <- data.frame(m=rep(NA,Mmax),v=rep(NA,Mmax),t=rep(NA,Mmax))

  #---------------------------------------------------

  pairs = Mmax/2;

  for(i in 1:pairs) {

    M = 2*i-1;
    lof_best <- Inf

    for(m in 1:M) {

      remaining_xvar = setdiff(1:n, Bfuncs[[m]][,2])

      for(v in remaining_xvar){

        tt <- split_points(x[,v],B[,m])

        for(t in tt) {

          Bnew <- data.frame(B[,(1:M)],
                             # Add pairs of basis functions
                             Btem1=B[,m]*h(x[,v],1,t),
                             Btem2=B[,m]*h(x[,v],-1,t))
          Bfuncs[[M+1]] = Bfuncs[[m]]
          Bfuncs[[M+2]] = Bfuncs[[m]]
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat,control)

          if(lof < lof_best) {
            lof_best <- lof
            splits[M,] <- c(m,v,t)
          }

        } #End loop over splits (t)

      } #End loop over variables (v)

    } #End loop over basis functions to split (m)

    #Save optimal (m, v, t) and update basis functions
    mstar <- splits[M,1]; vstar <- splits[M,2]; tstar <- splits[M,3]
    cat("[Info] best (m,v,t,lof): (",mstar,vstar,tstar,lof_best,")\n")

    B[,M+1] <- B[,mstar]*h(x[,vstar],-1,tstar) #Add left child
    B[,M+2] <- B[,mstar]*h(x[,vstar],1,tstar) #Add right child

    #Add pairs of basis functions
    Bfuncs[[M+1]] = rbind(Bfuncs[[mstar]], c(s=-1, v=vstar, t=tstar))
    Bfuncs[[M+2]] = rbind(Bfuncs[[mstar]], c(s=1, v=vstar, t=tstar))

  } # end loop over M

  colnames(B) = paste0("B",(0:(ncol(B)-1)))
  return(list(y=y,B=B,Bfuncs=Bfuncs))

}

#' Backwards Stepwise
#'
#' Implementation of algorithm 3 from page 17 of the Friedman paper
#'
#' @param fwd output created by running fwd_stepwise
#' @param control mars.control object, created with mars.control
#'
#' @return A list with elements
#' \describe{
#' \item{y The same vector of response values given as input}{}
#' \item{B }{}
#' \item{Bfuncs }{}
#' }
#'
#' @export
#'
#' @references
#' Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1),
#'  1–67. https://doi.org/10.1214/aos/1176347963
bwd_stepwise <- function(fwd,control) {

  #Guidance from lecture material

  Mmax = ncol(fwd$B)-1
  jstar = 2:(Mmax+1)
  kstar = jstar
  data = data.frame(y=fwd$y, fwd$B)
  lofstar = LOF(y~.-1, data, control)

  # Loop over M (outer loop)
  for(M in (Mmax+1):2){

    b = Inf # Best LOF
    L = kstar # Temp copy of kstar

    if(control$trace){
     cat("L:", L, "\n")
    }

    # Inner loop over model terms
    for(m in L){

      K = setdiff(L,m) # Removing mth basis function

      data_sub = data.frame(y=fwd$y, fwd$B[,K])
      lof = LOF(y~., data_sub, control) # Re-calculate LOF for new model

      # Update kstar with best LOF in this iteration
      if(lof < b){
        b = lof
        kstar = K
      }

      # Update jstar with best LOF of all iterations
      if(lof < lofstar){
        lofstar = lof
        jstar = K
      }

    }

  }

  jstar = c(1, jstar)
  #best model with indices of best model
  return(list(y=fwd$y, B=fwd$B[,jstar],Bfuncs=fwd$Bfuncs[jstar]))

}

#' Lack Of Fit
#'
#' Use GCV to calculate the the lack of fit of a model, following equation 30
#' on page 20 of the Friedman paper
#'
#' @param form formula to fit a linear model
#' @param data dataset to fit a model to
#' @param control a mars.control object created by mars.control
#'
#' @return lof, the lack of fit value for the linear model fit with the given data and formula
#' @export
#' @references
#' Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1),
#'  1–67. https://doi.org/10.1214/aos/1176347963

LOF <- function(form,data,control) {

  #Guidance from lecture material

  model <- lm(form,data)
  rss = (sum(residuals(model)^2))
  nrows = nrow(data)
  ncols = length(model$coefficients)-1
  c_tilde = sum(diag(hatvalues(model))) + control$d*ncols
  lof = rss * (nrows/(nrows - c_tilde)^2)
  return(lof)

}

#' Step Function
#'
#' Implementation of the step function defined on page 11 of the Friedman paper
#' @param x the data value being compared to the split point
#' @param s the side of the split point
#' @param t the split point
#'
#' @return if s=+1, this returns max(0,x-t); if s=-1, this return max(0,t-x)
#' @export
#' @references
#' Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1),
#'  1–67. https://doi.org/10.1214/aos/1176347963

h <- function(x,s,t) {
  # if x>t, s=+1, this return max(0,x-t)
  # if x<t, s=-1, this return max(0,t-x)
  return(pmax(0,s*(x-t)))
}

#' Split Points
#'
#' Find possible split points and sort them, removing the largest value
#'
#' @param xv Possible split points
#' @param Bm A basis function
#'
#' @return a sorted list of unique split points where the basis function is positive,
#' with the largest point removed
#' @export
#'
split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

#' Initialize B
#'
#' Initialize a matrix for all the basis functions
#'
#' @param N the number of points in each basis function
#' @param Mmax the maxium number of basis functions
#'
#' @return a data frame with an intercept column filled with ones and a column filled with
#' NA for each basis function.
#'
#' @export
#'
init_B <- function(N,Mmax) {
  B <- data.frame(matrix(NA,nrow=N,ncol=(Mmax+1)))
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}


#------------------------------------------------------------------------
# constructor, validator and helper for class mars.control
#------------------------------------------------------------------------
#

#' mars.control helper function
#'
#' Helper function to create a new mars.control object
#'
#' @param control a control object
#'
#' @return a mars.control object
#' @export
#'
new_mars.control <- function(control) {
  structure(control,class="mars.control")
}

#' Validator for `mars.control` objects
#'
#' Check that a mars.control object has valid parameters
#' @param control a mars.control object
#'
#' @return a mars.control object
#' @export
#'
validate_mars.control <- function(control) {
  stopifnot(is.integer(control$Mmax),
            is.numeric(control$d),
            is.logical(control$trace))
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2
  }
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- as.integer(2*ceiling(control$Mmax/2))
    warning("Mmax should be an even integer. Reset it to ",control$Mmax)
  }
  control
}


#' Constructor for `mars.control` objects
#'
#' This function constructs a `mars.control` object that specifies
#' parameters used in the model fitting procedure.
#'
#' @param Mmax Maximum number of basis functions. Should be an even integer. Default value is 2.
#' @param d A smoothing parameter
#' @param trace logical, if TRUE traceback??????
#' @export

mars.control <- function(Mmax=2,d=3,trace=FALSE) {
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}

# set.seed(123); n <- 10
# data <- data.frame(x1=rnorm(n),x2=rnorm(n), y=rnorm(n))
# y=rnorm(n)
# formula <- formula(y ~.)
# mc=mars.control()
# LOF(formula,data,mc)
#

