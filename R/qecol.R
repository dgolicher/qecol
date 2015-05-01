
#' Quickly run a regression with one command.
#' 
#' @param x Numerical vector.
#' @param y A Numerical vector.
#' @param xlab Label for x axis.
#' @param ylab Label for y axis.
#' @return Runs a quick regression and prints the results to a markdown report.
#' @examples
#' data(mussels)
#' q_regres(mussels$Lshell,mussels$BTVolume,"Shell length","Body volume")

q_regres<-function(x,y,xlab,ylab){
  require(ggplot2)
  d<-data.frame(x,y)
  g0<-ggplot(d,aes(x=x,y=y))
  g1<-g0+geom_point()+geom_smooth(method="lm")+theme_bw()+xlab(xlab)+ylab(ylab)
  print(g1)
  mod<-lm(y~x)
  print(summary(mod))
  print(confint(mod))
  par(mfcol=c(2,2))
  plot(mod)
}