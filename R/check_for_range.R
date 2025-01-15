#' Check for ramps/shutdowns within a univariate time series subset
#' 
#' @param data Data object returned by the loadData function
#' @param range Flat signed change in forecasted variable
#' @return Boolean, 1 = event happened, 0 = event did not happen
#' @export
check_for_range <- function(data,range){
  
  # Check if any (1) or none (0) of the values fall within the selected range
  result <- sign(sum((data >= range[1]) & (data <= range[2])))
  
  return(result)
  
}
