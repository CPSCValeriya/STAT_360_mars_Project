#'
#' @title Plot Diagnostics for a mars Object
#'
#' @description Four plots (selected by which) are available: a cdf plot of |residuals|, a fitted vs residual plot,
#' a q-q plot and a scale-location plot
#'
#' @param mars a mars object, typically the result of [mars()]
#' @param which a subset of the numbers 1:4 specifying which plots to create, by default 1:4
#'  \describe{
#'   \item{1}{"CDF" plot of absolute value of residuals}
#'   \item{2}{"Residual vs Fitted" plot}
#'   \item{3}{"Normal Q-Q" plot}
#'   \item{4}{"Scale-Location" plot}
#'   }
#' @param col color for: the line in the cdf plot or the points n the remaining points
#' @param numpoints how many of the largest absolute value points to plot
#' @param ... Extra parameters to pass to plotting functions
#'
#' @export


plot.mars<-function(mars,which=c(1,2,3,4),col="purple",numpoints=3,...){
  par(ask=T)
  class(mars) = class(mars)[2]; #get lm class

  if (1 %in% which){
    # CDF Plot
    cdf=ecdf(x=abs(mars$residuals))
    plot(cdf,do.points=F,xlab="abs(Residuals)",ylab = "Proportion",main="Cumulative Distribution")
    curve(cdf,from=0,to=max(abs(mars$residuals)),add=T,col=col)
  }

  if (2 %in% which){
    # Residual vs Fitted Plot
    plot(mars,which=1,col=col,id.n=numpoints)
  }

  if (3 %in% which){
    # Q-Q Plot
    plot(mars,which=2,col=col,id.n=numpoints)
  }

  if (4 %in% which){
    # Scale - Location Plot
    plot(mars,which=3,col=col,id.n=numpoints)
  }

  class(mars) = c("mars",class(mars))
}
