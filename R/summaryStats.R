#' Print summary statistics
#' 
#' @param data A list with two elements. The first element contains forecast data as a list, and the second contains observations.
#' @return Does not return anything, but simply prints the summary statistics on the screen.
#' @export
summaryStats <- function(data){

  f <- data$forecasts
  obs <- data$observations
  
  sumstat.forecList <- list()
  nfcfiles <- length(f)
  
  for(i in 1:nfcfiles){
    fc <- as.matrix(f[[i]][,-1])
    sumstat.forecList[[i]] <- data.frame(mean=mean(fc,na.rm=T),
                                         min=min(fc,na.rm=T),
                                         max=max(fc,na.rm=T),
                                         number_of_forecasts=dim(fc)[1],
                                         ensemble_size=dim(fc)[2])
    
  }
  names(sumstat.forecList) <- names(f)
  
  sumstat.obs <- list(data.frame(mean=mean(obs$obs,na.rm=T),
                                 min=min(obs$obs,na.rm=T),
                                 max=max(obs$obs,na.rm=T),
                                 number_of_observations=sum(!is.na(obs$obs)),
                                 missing_values=sum(is.na(obs$obs))))
  names(sumstat.obs) <- "observations.csv"
  
  # Print to R
  cat("OBSERVATIONS\n")
  cat("------------\n")
  print(head(obs))
  cat("\n\nFORECASTS\n")
  cat("---------\n")
  for(i in 1:nfcfiles){
    print(head(f[[i]][,1:c(min(4,dim(f[[i]])[2]))]))
  }
  cat("\n")
  print(sumstat.obs)
  print(sumstat.forecList)
    
}