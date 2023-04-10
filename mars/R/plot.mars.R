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
