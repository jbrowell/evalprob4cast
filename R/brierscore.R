#' Brier score
#'
#' @param pred_prob Predicted probabilities
#' @param binary_obs Binary observations
#' @return Brier score
#' @export

brierscore <- function(pred_prob,binary_obs){
  
  N <- length(pred_prob)
  brier <- 1/N*sum((pred_prob - binary_obs)^2)
  return(brier)
  
}
