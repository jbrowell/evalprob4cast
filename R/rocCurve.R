#' ROC Curve
#'
#' @param detect_table_list List of detection tables
#' @param threshold Required number of positive ensemble members
#' @param main title of ROC plot
#' @return Contingency table for all forecast candidates
#' @export

rocCurve <- function(detect_table,threshold=5,main=""){
  
  M_eval <- dim(detect_table)[2]-2
  detect_table_sum <- data.frame(obs=detect_table[,2],
                                 forecast=apply(as.matrix(detect_table[,-c(1,2)]),1,function(x){sum(x)/M_eval}))
  
  # Get binary table for obs vs. forecast (currently 5 positives needed for a detection)
  detect_table_ct <- detectToBinary(detect_table_sum,threshold=threshold/M_eval)

  # Prepare ROC curve object
  PRROC_obj <- roc.curve(scores.class0 = detect_table_sum[,2],weights.class0 = detect_table_sum[,1],curve=T)
  
  # Check that points can be drawn
  if( sum(abs(diff(PRROC_obj$curve[,3])))==0 ){
    
    plot(0,0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",main=main)
    text(0,0,"This forecast can not be converted into a ROC curve.",cex=1.3)
    
  }else{
    
    plot(PRROC_obj,main=main)
    
  }
  
  return(PRROC_obj)
  
}
