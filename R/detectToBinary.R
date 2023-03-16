#' Convert continuous classifier into binary classifier w.r.t a threshold
#'
#' @param tab Event detection table returned by the eventDetect function.
#' @param threshold A number.
#' @return Table of overall detection by a forecast candidate.
#' @export

detectToBinary <- function(tab,threshold){
  
  tab$forecast <- as.numeric(tab$forecast >= threshold)
  return(tab)
  
}
