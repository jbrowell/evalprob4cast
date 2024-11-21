#' LogS (univariate) of a probabilistic forecast w.r.t an observation.
#'
#' @param f Vector representing a probabilistic forecast.
#' @param y The corresponding observation.
#' @return Logarithmic score (LogS) of f and y.
#' @export

log_score <- function(f,y){
  
  -log(approxfun(density(f))(y))
  
}