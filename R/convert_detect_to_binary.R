#' Convert continuous classifier into binary classifier w.r.t a threshold
#'
#' @param tab Event detection table returned by the detect_event function.
#' @param threshold A continuous number between 0 and 1.
#' @return Table of overall detection by a forecast candidate.
#' @export

convert_detect_to_binary <- function(tab,threshold){
  
  tab$forecast <- as.numeric(tab$forecast >= threshold)
  return(tab)
  
}
