#' Function for constructing list of event detection tables.
#' 
#' @param data Data object returned by the loadData function.
#' @param change Flat signed change in forecasted variable.
#' @param window Duration of time window within which the change occurs, given in hours.
#' @return Returns an event detection table.
#' @export

eventDetectionTable <- function(data,change=-30,window=5,export.results=F){
  
  nfcfiles <- length(data$forecasts)
  
  detect_table_list <- list()
  for(i in 1:nfcfiles){
    dat_eval <- merge(data$observations,data$forecasts[[i]])
    detect_table <- data.frame()
    
    if("BaseTime" %in% names(dat_eval)){
      detect_table <- eventDetect(subset(dat_eval,select=-BaseTime),
                                  change=change,window=window)
      detect_table <- cbind(dat_eval$BaseTime,detect_table)
      colnames(detect_table)[1] <- "BaseTime"
      detect_table <- detect_table[,c(2,1,3:dim(dat_eval)[2])] #Reorder, maybe make generic later
    }else{
      detect_table <- eventDetect(dat_eval,change=change,window=window)
    }
    
    detect_table_list[[i]] <- detect_table[!is.na(rowMeans(Filter(is.numeric,detect_table))),]
    
    # Export detection table to results folder (should maybe be optional?)
    if(export.results){
      dir.create("results",showWarnings = F) # Creating results folder if it doesn't already exist
      write.csv(detect_table_list[[i]],paste0("results/detect_table",gsub("forecast","",names(data$forecasts)[i]),".csv"),row.names=F)
    }
  }
  names(detect_table_list) <- names(data$forecasts)
  
  return(detect_table_list)
  
}
