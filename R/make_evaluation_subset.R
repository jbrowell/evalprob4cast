#' Refine data to evaluatable dataset
#'
#' @param data A forecast-observation data structure.
#' @param lead_time The lead time of interest in units of hours. Defaults to 0, which corresponds to no lead time specified.
#' @return A forecast-observation data structure with only intersecting timestamps preserved.
#' @export

make_evaluation_subset <- function(data, lead_time = 0){
  
  f <- data$forecasts
  obs <- data$observations
  nfcfiles <- length(data$forecasts)
  
  # If a lead time is specified
  if(lead_time != 0){
    for(i in 1:nfcfiles){
      if(!("BaseTime" %in% names(f[[i]]))){
        cat(paste0("\033[0;", 33, "m",
                   "Error! One or more forecast tables are missing a BaseTime column."
                   ,"\033[0m","\n"))
        return(NULL)
      }else{
        f[[i]] <- f[[i]][get_lead_time(f[[i]]) == lead_time,]
      }
    }
  }
  
  # Find time points of intersection, where every forecast and observation is available
  ts_intersect <- obs$TimeStamp
  for(i in 1:nfcfiles){
    ts_intersect <- intersect(ts_intersect,f[[i]]$TimeStamp)
  }
  
  # Construct refined dataset
  obs_ev <- obs[as.numeric(obs$TimeStamp) %in% ts_intersect,]
  obs_ev <- obs_ev[!duplicated(obs_ev$TimeStamp),]
  f_ev <- list()
  for(i in 1:nfcfiles){
    f_ev[[i]] <- f[[i]][as.numeric(f[[i]]$TimeStamp) %in% ts_intersect,]
    #f_ev[[i]]$BaseTime <- NULL # added
    f_ev[[i]] <- f_ev[[i]][!duplicated(f_ev[[i]]$TimeStamp),]
  }
  
  newdata <- list()
  newdata$forecasts <- f_ev
  newdata$observations <- obs_ev
  names(newdata$forecasts) <- names(data$forecasts)
  
  return(newdata)
  
}
