#' Rank histograms of a list of forecast candidates.
#'
#' @param data A number.
#' @param nbins The desired number of bins. Defaults to m+1 bins (code 0).
#' @param seed Optional seed. Defaults to 0 (random seed every time).
#' @param main Title of the histogram.
#' @param xlab x-axis label. Defaults to "Transformed ranks".
#' @param ylab y-axis label. Defaults to "Frequency".
#' @return Rank histogram for every forecast candidate in the data structure.
#' @export

rank_histogram_list <- function(data,nbins=0,seed=0,xlab="Transformed ranks",ylab="Frequency"){
  
  f <- data$forecasts
  obs <- data$observations
  nfc <- length(f)
  fcnames <- names(data$forecasts)
  
  result <- list()
  
  for(i in 1:nfc){
    
    fc <- f[[i]]
    fc$BaseTime <- NULL
    dat_eval <- merge(obs,fc)
    result[[i]] <- rank_histogram(as.matrix(dat_eval[-c(1,2)]),dat_eval$obs,
                  nbins,seed,fcnames[i],xlab,ylab)
    
  }
  
  names(result) <- fcnames
  return(invisible(result))
  
}
