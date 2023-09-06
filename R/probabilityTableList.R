#' Converts a list of event detections tables into a list of probability tables. Intermediate function.
#'
#' @param detect_table_list List of detection tables
#' @return A number.
#' @export
probabilityTableList <- function(detect_table_list){
  
  prob_list <- lapply(detect_table_list,function(x){
    data.frame(TimeStamp=x$TimeStamp,
               obs=x$obs,
               prob=rowSums(as.matrix(x[,-c(1,2)]))/(dim(x)[2]-2))
    })
  
  return(prob_list)
  
}
