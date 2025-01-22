#' ROC curves for a list of event detections
#'
#' @param detect_table_list List of detection tables
#' @param main title of ROC plot
#' @return Contingency table for all forecast candidates
#' @export

roc_curve_list <- function(detect_table_list, main=NA, col.curve="black", col.auc="light gray", xlab="False positive rate", ylab="True positive rate"){
  
  # Works only if it is a list maybe
  nfcfiles <- length(detect_table_list)
  
  list_of_roc_curves <- list()
  
  if(is.na(main)){
    main = names(detect_table_list)
  }else{
    main = rep(main, nfcfiles)
  }
  
  for(i in 1:nfcfiles){
    
    # Load detection table, disregard all timestamps
    detect_table <- Filter(is.numeric, detect_table_list[[i]])
    
    M_eval <- dim(detect_table)[2]-1
    detect_table_prob <- data.frame(obs=detect_table[,1],
                                   forecast=apply(as.matrix(detect_table[,-1]), 1, function(x){sum(x)/M_eval}))
    
    # Save to list
    list_of_roc_curves[[i]] <- roc_curve(detect_table_prob, main = main[i], col.curve = col.curve, col.auc = col.auc, xlab = xlab, ylab = ylab)
    names(list_of_roc_curves)[i] <- names(detect_table_list)[i]
    
  }
  
  return(invisible(list_of_roc_curves))
  
}
