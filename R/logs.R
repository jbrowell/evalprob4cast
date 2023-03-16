#' LogS (univariate) of a probabilistic forecast w.r.t an observation.
#'
#' @param sim Vector representing a probabilistic forecast.
#' @param y The corresponding observation.
#' @return Logarithmic score (LogS) of f and y.
#' @export

logs <- function(f,y){
  
  -log(approxfun(density(f))(y))
  
}