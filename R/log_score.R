#' LogS (univariate) of a probabilistic forecast w.r.t an observation.
#'
#' @param f Vector representing a probabilistic forecast.
#' @param y The corresponding observation.
#' @return Logarithmic score (LogS) of f and y.
#' @export

log_score <- function(f,y){
  
  pdf <- density(f)
  pdf$y[pdf$y == 0] <- min(pdf$y[pdf$y != 0]) # Where pdf=0, set to lowest nonzero value
  pdf_corrected <- max(approxfun(pdf)(y), min(pdf$y)/10, na.rm = T)
  -log(pdf_corrected)
  
}
