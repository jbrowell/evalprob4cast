#' Forecast evaluation
#'
#' @param data A list containing forecasts and observations.
#' @return A table of scores for the dataset.
#' @export
forecastEvaluation <- function(data){
  
  f <- data$forecasts
  obs <- data$observations
  nfcfiles <- length(f)
  fcnames <- names(data$forecasts)
  
  feval.ref <- cbind(obs$TimeStamp,sapply(obs$obs,function(x){crps(obs$obs,x)}))
  feval.crps <- list()
  
  for(i in 1:nfcfiles){
    
    fc <- f[[i]]
    fc$BaseTime <- NULL
    dat.eval <- merge(obs,fc)
    crps.f <- apply(dat.eval[,-1],1,function(x){crps(x[-1],x[1])})
    feval.crps[[i]] <- cbind(dat.eval$TimeStamp,crps.f)
  }
  
  score_table <- data.frame(forecast=c(fcnames,"reference"),
                            CRPS=c(unlist(lapply(feval.crps,function(x){mean(x[,2],na.rm=T)})),mean(feval.ref[,2],na.rm=T)))
  
  return(score_table)
  
}
