#' Produce fanchart / quantile plot
#' 
#' @param m To be written in 2024.
#' @param main To be written in 2024.
#' @param xlab To be written in 2024.
#' @param ylab To be written in 2024.
#' @param xlim To be written in 2024.
#' @param ylim To be written in 2024.
#' @param dt To be written in 2024.
#' @param grid To be written in 2024.
#' @param col To be written in 2024.
#' @param dt.type To be written in 2024.
#' @return Does not return anything yet, but displays a quantile plot.
#' @export
quantilePlot <- function(m,main="Quantile Plot",xlab="Time",ylab="Quantity",xlim=0,ylim=0,
                     dt=NULL,grid=T,col=c("light blue", "dark blue"),dt.type="normal"){
  
  # Identical for quanplot and spagplot
  m <- as.matrix(m)
  if(is.null(dt)){
    dt <- 1:dim(m)[1]
  }
  
  xmax <- NULL
  if(xlim==0){
    xmax <- length(dt)
    xlim=c(dt[1],tail(dt,1))
  }else{
    xmax <- xlim
    xlim=c(dt[1],dt[xlim])
  }
  
  # The rest
  tile <- apply(m[1:xmax,],1,function(x){quantile(x,seq(0.025,0.975,0.025))})
  colfunc <- colorRampPalette(col)
  x <- c(dt[1:xmax],rev(dt[1:xmax]))
  
  if(ylim==0){
    ylim=range(tile)
  }else{
    ylim=c(0,ylim)
  }
  
  plot(c(-1000,-1000),xlim=xlim,ylim=ylim,
       main=main,xlab=xlab,ylab=ylab,xaxt="n")#,cex.main=2,cex.lab=1.4,cex.axis=1.2)
  
  if(grid==T){
    grid(lwd=2,nx=NA,ny=NULL)
  }
  
  ticks <- dt[seq(1,xmax,l=4)]
  if(dt.type=="normal"){
    axis(1,at=ticks,labels=ticks)
  }else if(dt.type=="ts"){
    axis(1,at=ticks,labels=format(ticks,"%Y-%m-%d %H:%M"))
  }
  #axis(1,at=dt[seq(1,19*12+1,by=12)],tck=1,lty=2,col="gray",labels=F)
  
  tiletot <- dim(tile)[1]
  for(i in 1:(tiletot/2 - 1)){
    polygon(x,c(tile[i,],rev(tile[tiletot-i,])),col=colfunc(tiletot/2)[i],border=F)
  }
  
}
