#' Plot score by lead time
#'
#' @param scoretable a table of scores returned from the evaluate_marginal_distribution function
#' @param main the title of the plot.
#' @param xlab the x-label.
#' @param ylab the y-label.
#' @param xlim the x-range. Selected automatically unless specified.
#' @param ylim the y-range. Selected automatically unless specified.
#' @return a plot of scores by leadtime with one curve for each forecast candidate.
#' @export

plot_score_by_leadtime <- function(scoretable,main="default",xlab="Lead time",ylab="default",xlim="default",ylim="default",legend=T,legend_pos="topleft"){
  
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
  
  # Labels
  if(main == "default"){
    main <- paste0(colnames(scoretable)[3]," by lead time")
  }
  if(ylab == "default"){
    ylab <- colnames(scoretable)[3]
  }
  
  # Plot
  plot(0,0,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,main=main,xaxt="n")
  for(i in 2:ncol){
    lines(x[,1],x[,i],col=i,lwd=2)
    points(x[,1],x[,i],col=i,lwd=2,pch=16)
  }
  if(legend){
    legend(legend_pos,legend=colnames(x)[-1],col=2:ncol,lty=1,lwd=2,bty="n")
  }
  axis(1, at = unique(scoretable$leadtime))
  
}
