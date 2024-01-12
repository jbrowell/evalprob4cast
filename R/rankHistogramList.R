#' Rank histograms of a list of forecast candidates.
#'
#' @param data A number. More parameters to do.
#' @return A number.
#' @export

rankHistogramList <- function(data,nbins=0,seed=0,xlab="Transformed ranks",ylab="Frequency"){
  
  f <- data$forecasts
  obs <- data$observations
  nfc <- length(f)
  fcnames <- names(data$forecasts)
  
  result <- list()
  
  for(i in 1:nfc){
    
    fc <- f[[i]]
    fc$BaseTime <- NULL
    dat.eval <- merge(obs,fc)
    result[[i]] <- rankHistogram(as.matrix(dat.eval[-c(1,2)]),dat.eval$obs,
                  nbins,seed,fcnames[i],xlab,ylab)
    
  }
  
  names(result) <- fcnames
  return(invisible(result))
  
}
