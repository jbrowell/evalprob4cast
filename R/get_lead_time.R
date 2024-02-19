#' Extract lead time from forecast data.
#'
#' @param forecast A forecast table or row containing at least the columns named TimeStamp and BaseTime, respectively.
#' @return Lead times of the input forecasts.
#' @export
get_lead_time <- function(forecast) {
  
  if("BaseTime" %in% names(forecast)){
    lead_time <- forecast$TimeStamp - forecast$BaseTime
    units(lead_time) <- "hours"
    return(lead_time)
  }else{
    NA
  }
  
}
