#' CRPS (univariate) of a probabilistic forecast w.r.t an observation.
#'
#' @param f Vector representing a probabilistic forecast.
#' @param y The corresponding observation.
#' @return Continuous Ranked Probability Score (CRPS) of f and y.
#' @export

crps <- function(f,y){
  
  ecdf3 <- ecdf(f)
  x <- c(sort(f[f<y]),y)
  x2 <- c(y,sort(f[f>y]))
  sum(ecdf3(x[-length(x)])^2*diff(x)) +
    sum((1-ecdf3(x2[-length(x2)]))^2*diff(x2))
  
}
