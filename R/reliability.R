#' Compute the data points for a reliability diagram
#'
#' @param pred_prob a vector of predicted probabilities.
#' @param binary_obs a vector of binary observations.
#' @param bins the vector of bins, given as the points of distinction between individual bins, including 0 and 1.
#' @return a data frame with the points used for creating the reliability diagram.
#' @export

reliability <- function(pred_prob,binary_obs,bins="default"){
  
  # Default bins if unspecified
  if(is.character(bins)){
    bins = c(0,0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95,1)
  }else{
    bins = c(0,bins,1)
  }
  
  # Allocate vectors for result
  obs_freq <- numeric(length(bins)-1)
  f_prob <- bins[-length(bins)] + diff(bins)/2 # Mid-point of each bin
  
  # Hack: set right end to 1.01
  bins[length(bins)] <- 1.01
  
  # Run through every bin
  for(i in 1:(length(bins)-1)){
    
    sub_id <- which((pred_prob >= bins[i]) & (pred_prob < bins[i+1]))
    if(length(sub_id)>0){
      obs_freq[i] <- sum(binary_obs[sub_id])/length(binary_obs[sub_id])
    }else{
      obs_freq[i] <- NA
    }
    
  }
  
  result <- data.frame(forecast_prob=f_prob,obs_rel_freq=obs_freq)
  
  return(result)
  
}
