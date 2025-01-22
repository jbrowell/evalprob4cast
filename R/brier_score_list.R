#' Brier scores for a list of event detections
#'
#' @param detections A list of detections, which can be either ensemble detection tables or probability tables
#' @param prob Logical argument indiciating whether the first argument are probabilities or ensemble detections.
#' @return A vector of Brier scores, one for each forecast model.
#' @export
brier_score_list <- function(detections,prob=F) {
  
  probability_table <- detections
  if(!prob){
    probability_table <- probability_table_list(detections)
  }
  
  result <- unlist(lapply(probability_table,function(x){brier_score(x$prob,x$obs)}))
  return(result)

}
