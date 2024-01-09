#' Reliability diagrams of a list of forecast candidates
#'
#' @param detections A list of detections, which can be either ensemble detection tables or probability tables
#' @param prob Logical argument indiciating whether the first argument are probabilities or ensemble detections.
#' @return hmm...
#' @export

reliabilityDiagramList <- function(detections,bins="default",method="classic",prob=F) {
  
  result <- 0
  probability_table <- detections
  if(!prob){
    probability_table <- probabilityTableList(detections)
  }
  
  if(method == "classic"){
    
    if(is.character(bins)){
      result <- lapply(probability_table,function(x){reliabilityDiagram(x$prob,x$obs,"default")})
    }else{
      result <- lapply(probability_table,function(x){reliabilityDiagram(x$prob,x$obs,bins)})
    }
    
    return(invisible(result))
    
  }else if(method == "CORP"){
    
    if(is.character(bins)){
      result <- suppressWarnings(lapply(probability_table,function(x){as.reliabilitydiag(x$prob,x$obs)}))
    }else{
      result <- suppressWarnings(lapply(probability_table,function(x){as.reliabilitydiag(x$prob,x$obs,
                                                                                         xvalues=bins)}))
    }
    
    return(result)
    
  }else{
    
    print("Error! Invalid method. Please select \"classic\" or \"CORP\".")
    return(NULL)
    
  }
  
}
