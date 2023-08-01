#' ROC Curve
#'
#' @param detect_table_sum detection table with probabilities
#' @param main title of ROC plot
#' @return Contingency table for all forecast candidates
#' @export

rocCurve <- function(detect_table_sum,main=""){

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
