#' Function for detecting all events in a multivariate time series
#' 
#' @param data Data object returned by the loadData function
#' @param change Flat signed change in forecasted variable
#' @param range ?
#' @param window Time window length within which the change occurs
#' @return Returns an event detection table
#' @export

detect_event <- function(data, change=NA, range=NA, window){
  
  # Only one event-argument allowed
  args_selected <- (!is.na(change)) + (!is.na(sum(range)))
  if(args_selected > 1){
    print("Please select only 1 event-argument (change/range)!")
    return(-1)
  }
  
  n <- length(data$TimeStamp)
  detect_table <- data
  
  # Run through every timestamp and associated X-hour forecast window
  for(i in 1:n){
    
    # Specify forecast-window-specific subset
    i2 <- max(which((data$TimeStamp - data$TimeStamp[i]) <= window*3600))
    evset <- data[i:i2,]
    
    # Check current forecast window for whether there is an event or not
    # (across every ensemble member + observations)
    if(dim(evset)[1]>1){
      if(!is.na(change)){
        detect_table[i,-1] <- apply(evset[,-1],2,function(x){check_for_change(x,change)})
      }else if(!is.na(sum(range))){
        detect_table[i,-1] <- apply(evset[,-1],2,function(x){check_for_range(x,range)})
      }
    }else{
      detect_table[i,-1] <- NA
    }
    
  }
  
  return(detect_table)
  
}
