#' Plot forecast
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
plotFc <- function(m,x=NULL,type="Quantile Plot",main=type,xlab="Time",ylab="Quantity",
                   grid=T,observations=NULL,
                   col=if(type %in% c("Quantile Plot","Fan Plot")){c("light blue", "dark blue")}else{"gray"}){
  
  if(type %in% c("Quantile Plot","Fan Plot")){
    
    quantilePlot(m=m,x=x,main=main,xlab=xlab,ylab=ylab,
                 grid=grid,col=col)
    
  }else if(type=="Spaghetti"){
    
    spaghettiPlot(m=m,x=x,main=main,xlab=xlab,ylab=ylab,
                  grid=grid,col=col)
    
  }else{
    cat("Invalid plot type, please type = 'Quantile Plot' or 'Spaghetti' \n")
    return(NULL)
  }
  
}
