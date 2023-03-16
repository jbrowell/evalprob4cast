#' Contingency table
#'
#' @param tab TBD
#' @return Contingency table for one forecast candidate
#' @export

contingencyTable <- function(tab){
  
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
