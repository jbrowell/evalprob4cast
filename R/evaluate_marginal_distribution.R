#' Forecast evaluation
#'
#' @param data A list containing forecasts and observations.
#' @param by_lead_time Logical indicator for whether forecast evaluation should be lead time-specific or not. Defaults to true.
#' @return A table of scores for the dataset.
#' @export
evaluate_marginal_distribution <- function(data, by_lead_time=T){
  
  f <- data$forecasts
  obs <- data$observations
  nfcfiles <- length(f)
  fcnames <- names(data$forecasts)
  
  feval.ref <- cbind(obs$TimeStamp,sapply(obs$obs,function(x){crps(obs$obs,x)}))
  feval.crps <- list()
  
  if(by_lead_time){
    
    # Logical, whether lead time is available or not
    has_lead_time <- unlist(lapply(data$forecasts,function(x){"BaseTime" %in% names(x)}))
    
    # All unique lead times across all forecast candidates
    unique_lead_times <- sort(unique(unlist(lapply(f,get_lead_time))),na.last=NA)
    
    for(i in 1:nfcfiles){
      
      feval.crps[[i]] <- list()
      
      if(!has_lead_time[i]){
        
        # Do nothing
        
      }else{
        
        fc <- f[[i]]
        all_lead_times <- get_lead_time(fc)
        
        for(j in 1:length(unique_lead_times)){
          
          fcu <- fc[all_lead_times==unique_lead_times[j],]
          
          if(dim(fcu)[1]==0){
            feval.crps[[i]][[j]] <- NA
          }
          else{
            fcu$BaseTime <- NULL
            dat.eval <- merge(obs,fcu)
            crps.f <- apply(dat.eval[,-1],1,function(x){crps(x[-1],x[1])})
            feval.crps[[i]][[j]] <- cbind(dat.eval$TimeStamp,crps.f)
          }
          
        }
      }
      
    }
    
    scores <- lapply(feval.crps,function(i){lapply(i,function(j){mean(j[,2],na.rm=T)})})
    #scores <- t(matrix(unlist(scores),ncol=sum(has_lead_time)))
    scores <- matrix(unlist(scores),ncol=1)
    
    #score_table <- cbind(data.frame(forecast=fcnames[has_lead_time],reference=mean(feval.ref[,2],na.rm=T)),scores)
    score_table <- cbind(data.frame(forecast=c(rep(fcnames[has_lead_time],each=length(unique_lead_times)),"reference"),
                                    leadtime=c(rep(unique_lead_times,sum(has_lead_time)),NA),
                                    CRPS=c(scores,mean(feval.ref[,2],na.rm=T))))
    #colnames(score_table)[-c(1,2)] <- paste0("CRPS_h",unique_lead_times)
    
    if(sum(!has_lead_time)>0){
      cat(paste0("\033[0;", 33, "m",
                 "NB! The following forecast models can not be separated by lead time:"
                 ,"\033[0m","\n"))
      cat(paste0("\033[0;", 33, "m",
                 names(which(!has_lead_time))
                 ,"\033[0m"),sep=", ")
      #cat("Warning! The following forecast models can not be separated by lead time:\n")
      #cat(names(which(!has_lead_time)),sep = ", ")
      cat("\n\n")
    }
    
    return(score_table)
    
  }else{
    
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
  
}
