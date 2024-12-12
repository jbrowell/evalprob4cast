#' Plot comparable variogram scores
#'
#' @param scoretable a table of scores returned from the forecastEvaluation function
#' @param main the title of the plot. Defaults to "Score by leadtime".
#' @param xlab the x-label. Defaults to "Leadtime".
#' @param ylab the y-label. Defaults to "Score".
#' @param xlim the x-range. Selected automatically unless specified.
#' @param ylim the y-range. Selected automatically unless specified.
#' @return a plot of scores by leadtime with one curve for each forecast candidate.
#' @export

plot_variogram_scores <- function(scoretable,main="Variogram score by basetime",xlab="Basetime",ylab="VarS",xlim="default",ylim="default",legend=T,legend_pos="topleft"){
  
  # Reshape
  scoretable <- scoretable[which(scoretable$dimension == max(scoretable$dimension,na.rm=T)),]
  main <- paste0(main,", dimension: ",max(scoretable$dimension,na.rm=T))
  scoretable$dimension <- NULL
  
  x <- reshape(subset(scoretable,forecast!="reference"), direction="wide", idvar="basetime", timevar="forecast")
  
  # Plot specs
  if(is.character(xlim)){
    xlim = range(x[,1])
  }
  if(is.character(ylim)){
    ylim = range(x[,-1], na.rm=T)
  }
  ncol <- dim(x)[2]
  
  # Plot
  plot(0,0,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,main=main,xaxt="n")
  for(i in 2:ncol){
    lines(x[,1],x[,i],col=i,lwd=2)
    points(x[,1],x[,i],col=i,lwd=2,pch=16)
  }
  if(legend){
    legend(legend_pos,legend=colnames(x)[-1],col=2:ncol,lty=1,lwd=2,bty="n")
  }
  
  t <- unique(scoretable$basetime)
  idx <- 1:length(t)
  while(length(idx) > 8){
    idx <- idx[seq(1,length(idx),by=2)]
  }
  xticks <- t[idx]
  
  axis(1, at = xticks, labels = xticks, cex.axis = 0.7)
  
}
