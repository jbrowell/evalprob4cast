#' Produce ROC curves for every forecast candidate in an evaluation set
#'
#' @param detect_table_list List of detection tables
#' @param main title of ROC plot
#' @return Contingency table for all forecast candidates
#' @export

roc_curve_list <- function(detect_table_list, main=""){
  
  # Works only if it is a list maybe
  nfcfiles <- length(detect_table_list)
  
  list_of_roc_curves <- list()
  
  for(i in 1:nfcfiles){
    
    # Load detection table, disregard all timestamps
    detect_table <- Filter(is.numeric, detect_table_list[[i]])
    
    M_eval <- dim(detect_table)[2]-1
    detect_table_prob <- data.frame(obs=detect_table[,1],
                                   forecast=apply(as.matrix(detect_table[,-1]), 1, function(x){sum(x)/M_eval}))
    
    # Save to list
    list_of_roc_curves[[i]] <- roc_curve(detect_table_prob, main=names(detect_table_list)[i])
    
  }
  
  return(list_of_roc_curves)
  
}
