#' Function for constructing list of event detection tables.
#' 
#' @param data Data object returned by the loadData function
#' @param change Flat signed change in forecasted variable
#' @param window Time window length within which the change occurs
#' @return Returns an event detection table
#' @export

eventDetectionTable <- function(data,change,window){
  
  nfcfiles <- length(data$forecasts)
  
  detect_table_list <- list()
  for(i in 1:nfcfiles){
    dat_eval <- merge(data$observations,data$forecasts[[i]])
    detect_table <- eventDetect(dat_eval,change=-30,window=4)
    detect_table_list[[i]] <- detect_table[!is.na(rowMeans(detect_table[,-1])),]
    
    # Export detection table to results folder (should maybe be optional?)
    write.csv(detect_table_list[[i]],paste0("results/detect_table",gsub("forecast","",names(data$forecasts)[i])),row.names=F)
  }
  names(detect_table_list) <- names(data$forecasts)
  
  return(detect_table_list)
  
}
