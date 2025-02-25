#' Print summary statistics
#' 
#' @param data A list with two elements. The first element contains the forecast data, and the second contains observations.
#' @return Does not return anything, but simply prints the summary statistics on the screen.
#' @export
summary_stats <- function(data){

  f <- data$forecasts
  obs <- data$observations
  
  sumstat.forecList <- list()
  nfcfiles <- length(f)
  
  for(i in 1:nfcfiles){
    fc <- f[[i]]
    # Only consider numerical columns for summary stats - now obsolete, just removed named columns
    # fc[,sapply(fc,class)!="numeric"] <- NULL
    fc$TimeStamp <- NULL
    fc$BaseTime <- NULL
    
    fc <- as.matrix(fc)
    sumstat.forecList[[i]] <- data.frame(mean=mean(fc,na.rm=T),
                                         min=min(fc,na.rm=T),
                                         max=max(fc,na.rm=T),
                                         number_of_forecasts=dim(fc)[1],
                                         ensemble_size=dim(fc)[2])
    
  }
  names(sumstat.forecList) <- names(f)
  
  # Ensure observation column is labeled "obs", may be changed later
  colnames(obs)[sapply(obs,class)=="numeric"] <- "obs"
  sumstat.obs <- list(data.frame(mean=mean(obs$obs,na.rm=T),
                                 min=min(obs$obs,na.rm=T),
                                 max=max(obs$obs,na.rm=T),
                                 number_of_observations=sum(!is.na(obs$obs)),
                                 missing_values=sum(is.na(obs$obs))))
  names(sumstat.obs) <- "observations"
  
  # Print to R
  cat("OBSERVATIONS\n")
  cat("------------\n")
  print(head(obs), row.names = F)
  cat("\n\nFORECASTS\n")
  cat("---------\n")
  for(i in 1:nfcfiles){
    cat(names(f)[i])
    cat("\n")
    print(head(f[[i]][,1:c(min(5,dim(f[[i]])[2]))]), row.names = F)
    cat("\n")
  }
  cat("\n")
  cat("SUMMARY\n")
  cat("-------\n")
  print(sumstat.obs, row.names = F)
  print(sumstat.forecList, row.names = F)
  
  return(invisible(NULL))
    
}
