#' Refine data to evaluatable dataset
#'
#' @param data Dataset.
#' @return A number.
#' @export

evaluationSet <- function(data){
  
  f <- data$forecasts
  obs <- data$observations
  nfcfiles <- length(data$forecasts)
  
  # Find time points of intersection, where every forecast and observation is available
  ts_intersect <- obs$TimeStamp
  for(i in 1:nfcfiles){
    ts_intersect <- intersect(ts_intersect,f[[i]]$TimeStamp)
  }
  
  # Construct refined dataset
  obs_ev <- obs[obs$TimeStamp %in% ts_intersect,]
  obs_ev <- obs_ev[!duplicated(obs_ev$TimeStamp),]
  f_ev <- list()
  for(i in 1:nfcfiles){
    f_ev[[i]] <- f[[i]][f[[i]]$TimeStamp %in% ts_intersect,]
    f_ev[[i]]$BaseTime <- NULL # added
    f_ev[[i]] <- f_ev[[i]][!duplicated(f_ev[[i]]$TimeStamp),]
  }
  
  newdata <- list()
  newdata$forecasts <- f_ev
  newdata$observations <- obs_ev
  names(newdata$forecasts) <- names(data$forecasts)
  
  return(newdata)
  
}
