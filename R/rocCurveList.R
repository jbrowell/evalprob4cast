#' ROC Curve
#'
#' @param detect_table List of detection tables
#' @param threshold Required number of positive ensemble members
#' @param main title of ROC plot
#' @return Contingency table for all forecast candidates
#' @export

rocCurveList <- function(detect_table_list,main=""){
  
  # Works only if it is a list maybe
  nfcfiles <- length(detect_table_list)
  
  roc_curve <- list()
  
  for(i in 1:nfcfiles){
    
    # Load detection table, disregard all timestamps
    detect_table <- Filter(is.numeric,detect_table_list[[i]])
    
    M_eval <- dim(detect_table)[2]-1
    detect_table_sum <- data.frame(obs=detect_table[,1],
                                   forecast=apply(as.matrix(detect_table[,-1]),1,function(x){sum(x)/M_eval}))
    
    # Save to list
    roc_curve[[i]] <- rocCurve(detect_table_sum,main=names(detect_table_list)[i])
    
  }
  
  return(roc_curve)
  
}
