plot.mars<-function(mars,...){
  par(mfrow=c(2,2))

  # CDF Plot
  cdf=ecdf(x=abs(mars$residuals))
  plot(cdf,do.points=F,xlab="abs(Residuals)",ylab = "Proportion",main="Cumulative Distribution")
  curve(cdf,from=0,to=max(abs(mars$residuals)),add=T)

  # Residual vs Fitted Plot
  class(mars) = class(mars)[2]; #get lm class
  plot(mars,which=1)

  # Q-Q Plot
  plot(mars,which=2)

  # Scale - Location Plot
  plot(mars,which=3)

  class(mars) = c("mars",class(mars))
}
