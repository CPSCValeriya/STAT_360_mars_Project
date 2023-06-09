#' Multivariate Adaptive Regression Splines (MARS)
#'
#' @title Multivariate Adaptive Regression Splines (MARS)
#' @description Implementation of Fit Friedman's Multivariate Adaptive Regression Splines (MARS) model. The Multivariate Adaptive Regression Spline is used to apply a regression model to a given set of data. In doing so, the function analyzes the dataset and creates numerous knots: points at which the fitted model will change.
#' @param formula an R formula
#' @param data a data frame containing the a response variable and predictors
#' @param control a mars.control object created using mars.control()
#' @return An S3 model of class "mars"
#' @references
#' \describe{
#'\item{Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1), 1–67. https://doi.org/10.1214/aos/1176347963}{}
#'\item{Lin, W. (2023). Lecture Material.}{}}
#' @seealso
#'\itemize{
#'\item{anova.mars}{}
#'\item{plot.mars}{}
#'\item{predict.mars}{}
#'\item{print.mars}{}
#'\item{summary.mars}{}}
#' @export
#' @examples
#'## Analyzing flower value data found in the iris dataset
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
#' @description Implements the Forward Stepwise algorithm (Algorithm 2 from page 17 of the
#' Friedman Paper) which  builds a linear prediction equation of linear basis functions which are the products of hinge functions.
#' @param y vector of response values
#' @param x dataset of predictor variables
#' @param control mars.control object created by the mars.control method
#' @return A list with elements: y, B, Bfuncs
#' \item{y}{the same vector of response values given as input}
#' \item{B}{final set of basis functions generated from the input}
#' \item{Bfuncs}{summary of the basis functions as a product of functions. Contains the information for the knot, variable, and sign}
#' @export
#' @references
#' \describe{
#'\item{Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1), 1–67. https://doi.org/10.1214/aos/1176347963}{}
#'\item{Lin, W. (2023). Lecture Material.}{}}
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

      remaining_xvar = setdiff(1:n, Bfuncs[[m]][,2]) #Exclude already used predictors

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

    #Print additional information if trace is true
    if(control$trace){
      cat("[Forward Stepwise] Best split: (basis: ",mstar,", variable: ", vstar, ", split point: ", round(tstar,2),", LOF: ", round(lof_best,2),")\n", sep="")
    }

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
#' @description Implements the Backwards Stepwise algorithm (Algorithm
#' 3 from page 17 of the Friedman paper) which combats overfitting of the forward stepwise algorithm by removing redundant basis functions until the generalized cross-validation criterion is satisfied.
#' @param fwd output created by running fwd_stepwise
#' @param control mars.control object, created with mars.control
#' @return A list with elements: y, B, Bfuncs
#' \item{y}{the same vector of response values given as input}
#' \item{B}{final set of basis functions generated from the input}
#' \item{Bfuncs}{summary of the basis functions as a product of functions. Contains the information for the knot, variable, and sign}
#' @export
#' @references
#' \describe{
#'\item{Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1), 1–67. https://doi.org/10.1214/aos/1176347963}{}
#'\item{Lin, W. (2023). Lecture Material.}{}}
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

        #Print additional information if trace is true
        if(control$trace){
          cat("[Backwards Stepwise] Updating basis set:", paste0("B",(jstar-1)), "\n")
        }

      }

    }

  }

  jstar = c(1, jstar)
  #best model with indices of best model
  return(list(y=fwd$y, B=fwd$B[,jstar],Bfuncs=fwd$Bfuncs[jstar]))

}

#' Lack Of Fit method
#'
#' @description Uses the generalized cross-validation criterion to calculate the lack of fit of a model, following equation 30 on page 20 of the Friedman paper.
#' @param form formula to fit a linear model
#' @param data dataset to fit a model to
#' @param control a mars.control object created by mars.control
#' @return lof, the lack of fit value for the linear model fit with the given data and formula
#' @export
#' @references
#' \describe{
#'\item{Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1), 1–67. https://doi.org/10.1214/aos/1176347963}{}
#'\item{Lin, W. (2023). Lecture Material.}{}}
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

#' Hinge Function
#'
#' @description Implementation of the hinge function defined on page 11 of the Friedman paper. Basis products of the mars object are a product of the hinge functions.
#' @param x the data value being compared to the split point
#' @param s the side of the split point
#' @param t the split point
#' @return if s=+1, this returns max(0,x-t); if s=-1, this return max(0,t-x)
#' @export
#' @references
#' \describe{
#'\item{Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1), 1–67. https://doi.org/10.1214/aos/1176347963}{}
#'\item{Lin, W. (2023). Lecture Material.}{}}
h <- function(x,s,t) {
  # if x>t, s=+1, this return max(0,x-t)
  # if x<t, s=-1, this return max(0,t-x)
  return(pmax(0,s*(x-t)))
}

#' Split Points
#'
#' @description Find possible split points and sort them, removing the largest value.
#' @param xv Possible split points
#' @param Bm A basis function
#' @return a sorted list of unique split points where the basis function is positive,
#' with the largest point removed
#' @export
#' @references
#' Lin, W. (2023). Lecture Material.
split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

#' Initialize B matrix
#'
#' @description Initializes an empty matrix to store the basis functions.
#' @param N the number of points in each basis function
#' @param Mmax the maxium number of basis functions
#' @return a data frame with an intercept column filled with ones and a column filled with
#' NA for each basis function.
#' @export
#' @references
#' Lin, W. (2023). Lecture Material.
init_B <- function(N,Mmax) {
  B <- data.frame(matrix(NA,nrow=N,ncol=(Mmax+1)))
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

#------------------------------------------------------------------------
# constructor, validator and helper for class mars.control
#------------------------------------------------------------------------

#' mars.control helper function
#'
#' @description Helper function to create a new mars.control object.
#' @param control a control object
#' @return a mars.control object
#' @export
#' @references
#' Lin, W. (2023). Lecture Material.
new_mars.control <- function(control) {
  structure(control,class="mars.control")
}

#' Validator for `mars.control` objects
#'
#' @description Checks that a mars.control object has valid parameters.
#' @param control a mars.control object
#' @return a mars.control object
#' @export
#' @references
#' Lin, W. (2023). Lecture Material.
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
#' @description This function constructs a `mars.control` object that specifies parameters used in the model fitting procedure.
#' @param Mmax Maximum number of basis functions. Should be an even integer. Default value is 2.
#' @param d A smoothing parameter. Default value is 3.
#' @param trace logical specifying whether more information should be printed to the user
#' @export
#' @references
#' Lin, W. (2023). Lecture Material.
mars.control <- function(Mmax=2,d=3,trace=FALSE) {
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}
