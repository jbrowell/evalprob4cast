#' Contingency table
#'
#' @param detect_table_binary A detection table of binary event forecasts (1 = event, 0 = no event).
#' @return A contingency table for a forecast candidate.
#' @export

contingency_table <- function(detect_table_binary){
  
  # Renaming for compact code
  tab <- detect_table_binary
  
  hits <- sum((tab$obs == 1) & (tab$forecast == 1))
  misses <- sum((tab$obs == 1) & (tab$forecast == 0))
  falsealarms <- sum((tab$obs == 0) & (tab$forecast == 1))
  correctnegatives <- sum((tab$obs == 0) & (tab$forecast == 0))
  ct <- data.frame(hits=hits,
                   misses=misses,
                   falsealarms=falsealarms,
                   correctnegatives=correctnegatives,
                   HR=round(hits/(hits+misses),3),
                   FAR=round(falsealarms/(falsealarms+correctnegatives),3))
  return(ct)
  
}
