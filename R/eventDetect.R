#' Function for detecting all events in a multivariate time series
#' 
#' @param data Data object returned by the loadData function
#' @param ch Flat signed change in forecasted variable
#' @param win Time window length within which the change occurs
#' @return Returns an event detection table
#' @export
eventDetect <- function(data,ch,win){
  
  n <- length(data$TimeStamp)
  detect_table <- data
  
  # Run through every timestamp and associated X-hour forecast window
  for(i in 1:n){
    
    # Specify forecast-window-specific subset
    i2 <- max(which((data$TimeStamp - data$TimeStamp[i]) <= win*3600))
    evset <- data[i:i2,]
    
    # Check current forecast window for whether there is an event or not
    # (across every ensemble member + observations)
    if(dim(evset)[1]>1){
      detect_table[i,-1] <- apply(evset[,-1],2,function(x){checkForEvent(x,ch)})
    }else{
      detect_table[i,-1] <- NA
    }
    
  }
  
  return(detect_table)
  
}