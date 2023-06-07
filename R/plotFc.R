#' Plot forecast
#' 
#' @param data Data object returned by the loadData function
#' @param f_id forecast candidate
#' @param style fanchart or spaghetti, defaults to fanchart
#' @param xmax Maximum number of observations plotted
#' @param main To be written in 2024.
#' @param xlab To be written in 2024.
#' @param ylab To be written in 2024.
#' @param grid To be written in 2024.
#' @param col To be written in 2024.
#' @param dt.type To be written in 2024.
#' @return Does not return anything yet, but displays a quantile plot.
#' @export
plotFc <- function(data,f_id,style="fanchart",xmax=100,main="Forecast",xlab="Time",ylab="Quantity",xlim=0,ylim=0,
                     grid=T,col=NULL,dt.type="normal"){
  
  f <- data$forecasts[[f_id]]
  obs <- data$observations
  dt <- f$TimeStamp
  
  m <- f
  m$BaseTime <- NULL
  m$TimeStamp <- NULL
  
  if(style=="fanchart"){
    
<<<<<<< Updated upstream
    if(is.null(col)){
      col <- c("light blue", "dark blue")
    }
    if(main == "Forecast"){
      main <- "Forecast quantiles"
=======
    quantilePlot(m=m,x=x,main=main,xlab=xlab,ylab=ylab,
                 grid=grid,col=col)
    if(!is.null(observations)){
      lines(x=x,y=observations)
>>>>>>> Stashed changes
    }
    
    quantilePlot(m,main=main,xlab=xlab,ylab=ylab,
                 xlim=xmax,
                 ylim=ylim,dt=dt,
                 grid=grid,col=col,dt.type=dt.type)
    
<<<<<<< Updated upstream
  }else if(style=="spaghetti"){
    
    if(is.null(col)){
      col <- "gray"
    }
    if(main == "Forecast"){
      main <- "Forecast ensemble"
    }
    
    spaghettiPlot(m,main=main,xlab=xlab,ylab=ylab,
                 xlim=xmax,
                 ylim=ylim,dt=dt,
                 grid=grid,col=col,dt.type=dt.type)
=======
    spaghettiPlot(m=m,x=x,main=main,xlab=xlab,ylab=ylab,
                  grid=grid,col=col)
    if(!is.null(observations)){
      lines(x=x,y=observations)
    }
>>>>>>> Stashed changes
    
  }else{
    cat("Invalid style, please select 'fanchart' or 'spaghetti' \n")
    return(NULL)
  }
  
  # Add observations
  lines(obs$TimeStamp,obs$obs,type="b")
  #if(is.null(dt)){
  #  cat("In order to plot observations, argument 'dt' must be specified!\n")
  #}else{
    
  #}
  
}
