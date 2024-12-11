#' Plot probabilistic forecasts
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
plot_forecasts <- function(m,x=NULL,type="Quantile Plot",grid=F,
                           all=F,numforec=100,by="default",lead=1,main=type,xlab="Time",ylab="Quantity",
                           col=if(type %in% c("Quantile Plot","Fan Plot")){c("light blue", "dark blue")}else{"gray"}){
  
  # Select by leadtime, basetime or default
  if("BaseTime" %in% names(m)){
    
    if(by == "leadtime"){
      
      if(length(which(get_lead_time(m) == lead)) == 0){
        lead_time <- max(0,min(get_lead_time(m)))
      }else{
        lead_time <- lead
      }
      
      main <- paste0(main,", lead: ",lead_time, "h")
      m <- m[get_lead_time(m) == lead_time,]
      
    }else if(by == "basetime"){
      
      base_time <- min(m$BaseTime)
      main <- paste0(main,", run: ",base_time)
      m <- m[m$BaseTime == base_time,]
      
    }else{ # what should be default?
      
      m <- m[!duplicated(m$TimeStamp),]
      m <- m[order(m$TimeStamp),]
      
    }
  }
  
  # Plot only the first n forecasts
  if(all == F){
    
    if(dim(m)[1] > numforec){
      m <- m[1:numforec,]
    }
  }
  
  x <- m$TimeStamp
  m$BaseTime <- NULL
  m$TimeStamp <- NULL
  
  # Make quantiles or spaghetti plot
  if(type %in% c("Quantile Plot","Fan Plot")){
    
    plot_quantiles(m=m,x=x,main=main,xlab=xlab,ylab=ylab,
                 grid=grid,col=col)
    
  }else if(type %in% c("Scenarios")){
    
    plot_scenarios(m=m,x=x,main=main,xlab=xlab,ylab=ylab,
                  grid=grid,col=col)
    
  }else{
    cat("Invalid plot type, please type = 'Quantile Plot', 'Fan Plot' or 'Scenarios' \n")
    return(NULL)
  }
  
}
