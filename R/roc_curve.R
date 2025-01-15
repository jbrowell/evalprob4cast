#' Produce a ROC curve for 
#'
#' @param detect_table_prob detection table with event probabilities
#' @param main title of ROC plot
#' @return Contingency table for all forecast candidates
#' @export

roc_curve <- function(detect_table_prob, main="ROC curve", col.curve="black", col.auc="light gray", xlab="False positive rate", ylab="True positive rate"){

  # Prepare ROC curve object
  PRROC_obj <- roc.curve(scores.class0 = detect_table_prob[,2], weights.class0 = detect_table_prob[,1], curve=T)
  roc <- list(curve = PRROC_obj$curve, auc = PRROC_obj$auc)
    
  # Check that points can be drawn
  if(sum(abs(diff(roc$curve[,3])))==0){
    
    plot(0, 0, type="n", xaxt="n", yaxt="n", xlab="", ylab="", main = main)
    text(0, 0, "This forecast can not be converted into a ROC curve.", cex=1.3)
    
  }else{
    
    invisible(par(mar=c(3.5,3.2,2.5,1)+0.1,mgp=c(1.9,0.7,0)))
    plot(roc$curve[,1], roc$curve[,2], type="l", lwd=2, main = main, col = col.curve, xlab = xlab, ylab = ylab)
    polygon(c(roc$curve[,1],1), c(roc$curve[,2],0), col = col.auc)
    lines(c(0,1), c(0,1), lty=2, lwd=1)
    text(0.75, 0.3, paste0("AUC = ",round(roc$auc,3)), adj = 0.5, cex=1.3)
    
    # Reset plot parameters to default
    par(mar=c(5,4,4,2)+0.1,mgp=c(3,1,0))
    
  }
  
  return(roc)
  
}
