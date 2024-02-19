#' VarS of a multivariate probabilistic forecast w.r.t a vector of observations.
#'
#' @param f Vector representing a probabilistic forecast.
#' @param y The corresponding observation.
#' @param p The order. Defaults to 0.5.
#' @return Variogram score (VarS) of f and y.
#' @export

variogram_score <- function(f, y, p){
  
  n <- dim(f)[2] # Size of ensemble
  d <- dim(f)[1] # Dimension
  
  # Should we check that length of observation vector matches?
  
  # Iterate through all pairs
  score <- 0
  for(i in 1:(d-1)){
    for(j in (i+1):d){
      
      w <- 1/abs(i-j)
      
      Ediff <- 1/n*sum(abs(f[i,]-f[j,])^p)
      score <- score + w * (abs(y[i]-y[j])^p - Ediff)^2
      
    }
  }
  
  # Result
  return(score)
}
