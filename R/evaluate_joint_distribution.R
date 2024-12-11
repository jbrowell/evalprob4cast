#' Forecast evaluation of the joint distribution using the variogram score
#'
#' @param data A list containing forecasts and observations.
#' @param by_base_time Logical indicator for whether forecast evaluation should be base time-specific or not. Defaults to true.
#' @return A table of scores for the dataset.
#' @export
evaluate_joint_distribution <- function(data, by_base_time=T, p=0.5){
  
  f <- data$forecasts
  obs <- data$observations
  nfcfiles <- length(f)
  fcnames <- names(data$forecasts)
  
  #feval.ref <- cbind(obs$TimeStamp,sapply(obs$obs,function(x){crps(obs$obs,x)}))
  feval.vars <- list()
  
  if(by_base_time){
    
    # Logical, whether each base time has a multivariate forecast or not
    has_base_time <- unlist(lapply(data$forecasts,function(x){"BaseTime" %in% names(x)}))
    
    # All unique lead times across all forecast candidates
    unique_base_times <- unique(do.call("c",lapply(f,function(x){unique(x$BaseTime)})))
    
    for(i in 1:nfcfiles){
      
      feval.vars[[i]] <- data.frame(BaseTime=unique_base_times,VarS=NA,dimension=NA)
      
      if(!has_base_time[i]){
        
        # Do nothing
        
      }else{
        
        fc <- f[[i]]
        all_base_times <- fc$BaseTime
        
        for(j in 1:length(unique_base_times)){
          
          fcu <- fc[all_base_times==unique_base_times[j],]
          
          if(dim(fcu)[1]==0){
            #feval.vars[[i]]$VarS[j] <- NA
          }
          else{
            current_base_time <- fcu$BaseTime[1]
            fcu$BaseTime <- NULL
            dat.eval <- merge(obs,fcu)
            if(dim(dat.eval)[1]==0){
              #feval.vars[[i]]$VarS[j] <- NA
            }else{
              vars.f <- variogram_score(dat.eval[,-c(1,2)], dat.eval$obs, p=p)
              feval.vars[[i]]$VarS[j] <- vars.f
              feval.vars[[i]]$dimension[j] <- dim(dat.eval)[1]
            }
          }
          
        }
      }
      
    }
    
    scores <- lapply(feval.vars,function(i){i$VarS})
    scores <- matrix(unlist(scores),ncol=1)
    dimensions <- matrix(unlist(lapply(feval.vars,function(i){i$dimension})), ncol=1)
    
    #score_table <- cbind(data.frame(forecast=fcnames[has_lead_time],reference=mean(feval.ref[,2],na.rm=T)),scores)
    score_table <- cbind(data.frame(forecast=c(rep(fcnames[has_base_time],each=length(unique_base_times)),"reference"),
                                    basetime=c(rep(unique_base_times,sum(has_base_time)),NA),
                                    VarS=c(scores,NA),
                                    dimension=c(dimensions,NA)))
    #colnames(score_table)[-c(1,2)] <- paste0("CRPS_h",unique_lead_times)
    
    if(sum(!has_base_time)>0){
      cat(paste0("\033[0;", 33, "m",
                 "NB! The following forecast models can not be separated by base time:"
                 ,"\033[0m","\n"))
      cat(paste0("\033[0;", 33, "m",
                 names(which(!has_base_time))
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
      dat.eval <- dat.eval[1:25,]
      vars.f <- variogram_score(dat.eval[,-c(1,2)], dat.eval$obs, p=p)
      feval.vars[[i]] <- c(vars.f,dim(dat.eval)[1])
    }
    
    score_table <- data.frame(forecast=c(fcnames,"reference"),
                              VarS=c(unlist(lapply(feval.vars,function(x){x[1]})),NA),
                              dimension=c(unlist(lapply(feval.vars,function(x){x[2]})),NA))
    return(score_table)
    
  }
  
}
