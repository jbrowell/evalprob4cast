#' Rank histogram of a series of ensemble forecasts w.r.t a series of observations.
#'
#' @param f Vector representing a series of ensemble forecasts.
#' @param y The corresponding observation series.
#' @param nbins The desired number of bins. Defaults to m+1 bins (code 0).
#' @param seed Optional seed. Defaults to 0 (random seed every time).
#' @param main Title of the histogram.
#' @param xlab x-axis label. Defaults to "Transformed ranks".
#' @param ylab y-axis label. Defaults to "Frequency".
#' @return Rank histogram.
#' @export

rank_histogram <- function(f,y,nbins=0,seed=0,main="Rank histogram",xlab="Transformed ranks",ylab="Frequency"){
  
  # If a data frame has been submitted, remove time columns and convert to matrix
  if(class(f)[1] == "data.frame"){
    f$TimeStamp <- NULL
    f$BaseTime <- NULL
    f <- as.matrix(f)
  }
  
  m <- dim(f)[2]
  if(nbins==0){
    nbins <- m + 1
  }
  
  pair_matrix <- cbind(y,f)
  
  r <- unlist(apply(pair_matrix,1,function(x){
    which(sort(x)==x[1])[max(1,round(length(which(sort(x)==x[1]))/2))]
  }))
  attr(r,"names") <- NULL
  
  if(seed==0){
    set.seed(as.numeric(Sys.time()))
  }else{
    set.seed(seed)
  }
  
  r_transformed <- (r - 1 + runif(length(r))) / (m+1)
  rhist <- hist(r_transformed,breaks=nbins,main=main,xlab=xlab,ylab=ylab)
  
  return(invisible(rhist))
  
}
