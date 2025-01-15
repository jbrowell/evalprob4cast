#' Produce fanchart / quantile plot
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
plot_quantiles <- function(m,x=NULL,main="Quantile Plot",xlab="Time",ylab="Quantity",
                     grid=T,col=c("light blue", "dark blue"),...){
  
  # Prepare data for plotting
  m <- as.matrix(m)
  if(is.null(x)){
    x <- 1:dim(m)[1]
  }
  
  # Calculate quantiles
  tile <- apply(m,1,function(x){quantile(x,seq(0.025,0.975,0.025))})
  
  # Prepare plot area
  colfunc <- colorRampPalette(col)
  plot(x=rep(x,each=2),
       y=c(apply(tile,2,max),apply(tile,2,min)),
       type="n",
       main=main,xlab=xlab,ylab=ylab,...)#,cex.main=2,cex.lab=1.4,cex.axis=1.2)
  
  # Add grid
  if(grid==T){
    grid(lwd=2,nx=NA,ny=NULL)
  }
  
  # Add fan plot
  if(dim(m)[2] > 1){
    tiletot <- dim(tile)[1]
    for(i in 1:(tiletot/2 - 1)){
      polygon(x = c(x,rev(x)),y = c(tile[i,],rev(tile[tiletot-i,])),
              col=colfunc(tiletot/2)[i],border=F)
    }
  }else{
    lines(x, m[,1], lwd=2, col=col[2])
  }
  
}
