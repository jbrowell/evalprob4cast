#' Converts a list of event detections tables into a list of probability tables. Intermediate function.
#'
#' @param detect_table_list A list of detection tables.
#' @return A list of probability tables.
#' @export
probability_table_list <- function(detect_table_list){
  
  list_of_probs <- lapply(detect_table_list, function(x){
    data.frame(TimeStamp=x$TimeStamp,
               obs=x$obs,
               prob=rowSums(as.matrix(x[,-c(1,2)]))/(dim(x)[2]-2))
    })
  
  return(list_of_probs)
  
}
