#' Plot observations
#' 
#' @param data a data frame of univariate observations.
#' @param x Optional time stamps corresponding to rows of \code{data}.
#' @param main a main title for the plot, see also \code{?plot}.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param grid Add a grid to the plot background?
#' @param col Color of the observations.
#' @param all Logical, if all observations should be drawn. Defaults to false, in which case up to 100 observations are shown.
#' @param ... Additional arguments passed to \code{plot()}
#' @return Does not return anything, but displays a quantile plot.
#' @export
plot_observations <- function(data,x=NULL,grid=F,col="black",
                              all=F,numobs=100,main="Observations",xlab="Time",ylab="Quantity"){

  if(all == F){
    if(dim(data)[1] > numobs){
      data <- data[1:numobs,]
    }
  }
  
  plot(data$TimeStamp, data$obs, type="n", xaxt="n",
       main=main, xlab="", ylab=ylab)
  
  # Choose xticks
  idx <- 1:length(data$TimeStamp)
  #idx <- which(format(data$TimeStamp,"%H:%M")=="00:00")
  while(length(idx) > 16){
    idx <- idx[seq(1,length(idx),by=2)]
  }
  xticks <- data$TimeStamp[idx]
  
  axis(1, at = xticks, labels = F, side = 1)
  
  yr <- range(data$obs, na.rm=T)
  yr_len <- yr[2] - yr[1]
  yr[1] <- yr[1] - yr_len
  yr[2] <- yr[2] + yr_len
  
  if(grid){
    invisible(sapply(1:length(xticks), function(x){lines(rep(xticks[x], 2), yr, col="light gray", lty=3)}))
    grid(nx = NA, ny = NULL) 
  }
  
  text(x = xticks, y = par("usr")[3] - 0.04*yr_len, labels = format(xticks, "%Y-%m-%d %H:%M"),
       xpd = NA, srt = 35, adj = 0.965, cex = 0.7)
  mtext(xlab, side = 1, line = 4)
  
  lines(data$TimeStamp, data$obs, type="l", col=col)
  points(data$TimeStamp, data$obs, col=col, cex=0.8)
  
}
