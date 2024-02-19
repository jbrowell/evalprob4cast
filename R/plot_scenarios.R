#' Produce scenario / spaghetti plot
#' 
#' @param m Ensemble forecast data
#' @param x Optional time stamps corresponding to rows of \code{m}.
#' @param main a main title for the plot, see also \code{?plot}.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param grid Add a grid to the plot background?
#' @param col Ends of the colour range for fan plot.
#' @param ... Additional arguments passed to \code{plot()}
#' @return Does not return anything, but displays a quantile plot.
#' @export
plot_scenarios <- function(m,x=NULL,main="Spaghetti Plot",xlab="Time",ylab="Quantity",
                          grid=T,col="gray",...){
  
  # Identical for quanplot and spagplot
  m <- as.matrix(m)
  if(is.null(x)){
    x <- 1:dim(m)[1]
  }
  
  
  plot(x=range(x),y=c(0,max(m)),
       main=main,xlab=xlab,ylab=ylab,
       type="n",...)#,cex.main=2,cex.lab=1.4,cex.axis=1.2)

  if(grid==T){
    grid(lwd=2,nx=NA,ny=NULL)
  }
  
  # invisible(apply(m,2,function(y){lines(x,y,col=col)}))
  for(i in 1:dim(m)[2]){
    lines(x,m[,i],col=col)
  }
  
}
