#' Contingency table list
#'
#' @param detect_table_list List of detection tables
#' @param threshold Required number of positive ensemble members
#' @return Contingency table for all forecast candidates
#' @export

contingencyTableList <- function(detect_table_list,threshold=0.75){
  
  # Works only if it is a list maybe
  nfcfiles <- length(detect_table_list)
  
  contingency_table <- list()
  for(i in 1:nfcfiles){
    
    # Load detection table, disregard all timestamps
    detect_table <- Filter(is.numeric,detect_table_list[[i]])

    # Prepare scores for binary classifier
    M_eval <- dim(detect_table)[2]-1
    detect_table_sum <- data.frame(obs=detect_table$obs,
                                   forecast=apply(as.matrix(detect_table[,-1]),1,function(x){sum(x)/M_eval}))
    
    # Get binary table for obs vs. forecast (currently by default 5 positives needed for a detection)
    detect_table_ct <- detectToBinary(detect_table_sum,threshold)
    
    # Make contingency table
    contingency_table[[i]] <- contingencyTable(detect_table_ct)
    
  }
  names(contingency_table) <- names(detect_table_list)
  
  return(contingency_table)
  
}
