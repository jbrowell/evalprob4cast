#' Check for ramps/shutdowns within a univariate time series subset
#' 
#' @param data A vector of numerical values
#' @param ch Flat signed change in forecasted variable
#' @return Boolean, 1 = event happened, 0 = event did not happen
#' @export
check_for_change <- function(data,ch){
  
  # Run through all forward pairs within subset
  n <- length(data)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      # Check if current pair constitutes an event
      ev_discrim <- as.numeric((data[j] - data[i])*sign(ch) >= abs(ch))
      if(is.na(ev_discrim)){
        ev_discrim <- 0
      }
      if(ev_discrim){
        # Return "event found" if the pair constitutes and event
        return(1)
      }
    }
  }
  
  # Return "no event" if no event was found anywhere in the subset
  return(0)
  
}