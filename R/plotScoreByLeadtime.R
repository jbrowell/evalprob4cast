#' Plot score by lead time
#'
#' @param scoretable a table of scores returned from the forecastEvaluation function
#' @param main the title of the plot. Defaults to "Score by leadtime".
#' @param xlab the x-label. Defaults to "Leadtime".
#' @param ylab the y-label. Defaults to "Score".
#' @param xlim the x-range. Selected automatically unless specified.
#' @param ylim the y-range. Selected automatically unless specified.
#' @return a plot of scores by leadtime with one curve for each forecast candidate.
#' @export

plotScoreByLeadtime <- function(scoretable,main="Score by leadtime",xlab="Leadtime",ylab="Score",xlim="default",ylim="default"){
  
  # Reshape
  x <- reshape(subset(scoretable,forecast!="reference"), direction="wide", idvar="leadtime", timevar="forecast")
  
  # Plot specs
  if(is.character(xlim)){
    xlim = range(x[,1])
  }
  if(is.character(ylim)){
    ylim = range(x[,-1])
  }
  ncol <- dim(x)[2]
  
  # Plot
  plot(0,0,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,main=main,xaxt="n")
  for(i in 2:ncol){
    lines(x[,1],x[,i],col=i,lwd=2)
    points(x[,1],x[,i],col=i,lwd=2,pch=16)
  }
  legend("topleft",legend=colnames(x)[-1],col=2:ncol,lty=1,lwd=2)
  axis(1, at = unique(scoretable$leadtime))
  
}
