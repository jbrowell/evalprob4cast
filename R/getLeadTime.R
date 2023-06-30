#' Add together two numbers
#'
#' @param forecast_row A row from a forecast table containing at least TimeStamp and BaseTime.
#' @return Lead time of a forecast row.
#' @export
getLeadTime <- function(forecast_row) {
  
  lead_time <- forecast_row$TimeStamp - forecast_row$BaseTime
  units(lead_time) <- "hours"
  return(lead_time[[1]])
  
}
