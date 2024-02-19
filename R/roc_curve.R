#' Produce a ROC curve for 
#'
#' @param detect_table_prob detection table with event probabilities
#' @param main title of ROC plot
#' @return Contingency table for all forecast candidates
#' @export

roc_curve <- function(detect_table_prob, main=""){

  # Prepare ROC curve object
  PRROC_obj <- roc.curve(scores.class0 = detect_table_prob[,2], weights.class0 = detect_table_prob[,1], curve=T)
  
  # Check that points can be drawn
  if(sum(abs(diff(PRROC_obj$curve[,3])))==0){
    
    plot(0,0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",main=main)
    text(0,0,"This forecast can not be converted into a ROC curve.",cex=1.3)
    
  }else{
    
    plot(PRROC_obj,main=main)
    
  }
  
  return(PRROC_obj)
  
}
