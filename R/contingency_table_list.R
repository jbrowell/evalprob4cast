#' List of contingency tables
#'
#' @param detect_table_list A list of event detection tables.
#' @param threshold Required number of positive ensemble members. Defaults to 0.75.
#' @return Contingency tables for all forecast candidates
#' @export

contingency_table_list <- function(detect_table_list,threshold=0.75){
  
  # Works only if it is a list maybe
  nfcfiles <- length(detect_table_list)
  
  list_of_contingency_tables <- list()
  for(i in 1:nfcfiles){
    
    # Load detection table, disregard all timestamps
    detect_table <- Filter(is.numeric,detect_table_list[[i]])

    # Prepare scores for binary classifier
    M_eval <- dim(detect_table)[2]-1
    detect_table_sum <- data.frame(obs=detect_table$obs,
                                   forecast=apply(as.matrix(detect_table[,-1]),1,function(x){sum(x)/M_eval}))
    
    # Get binary table for obs vs. forecast (currently by default 5 positives needed for a detection)
    detect_table_binary <- convert_detect_to_binary(detect_table_sum,threshold)
    
    # Make contingency table
    list_of_contingency_tables[[i]] <- contingency_table(detect_table_binary)
    
  }
  names(list_of_contingency_tables) <- names(detect_table_list)
  
  return(list_of_contingency_tables)
  
}
