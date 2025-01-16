#' Reliability diagrams of a list of forecast candidates
#'
#' @param detections A list of detections, which can be either ensemble detection tables or probability tables
#' @param bins the vector of bins, given as the points of distinction between individual bins, including 0 and 1. The default binning is selected by setting this argument to "default".
#' @param method either classic approach ("classic") or CORP ("CORP"). Defaults to "classic".
#' @param prob Logical argument indiciating whether the first argument are probabilities or ensemble detections.
#' @return hmm...
#' @export

reliability_diagram_list <- function(detections,bins="default",method="classic",prob=F) {
  
  result <- list()
  probability_table <- detections
  if(!prob){
    probability_table <- probability_table_list(detections)
  }
  
  nfc <- length(probability_table)
  fcnames <- names(probability_table)
  
  if(method == "classic"){
    
    if(is.character(bins)){
      bins = c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95)
    }
    
    for(i in 1:nfc){
      
      p <- probability_table[[i]]
      result[[i]] <- reliability_diagram(p$prob,p$obs,bins,main=fcnames[i])
      
    }
    
    names(result) <- fcnames
    return(invisible(result))
    
  }else if(method == "CORP"){
    
    if(!suppressMessages(require("reliabilitydiag"))){
      stop("In order to use the CORP method, please run install.packages(\"reliabilitydiag\"). The installation has many dependencies and may take a while on a fresh system.\n")
    }
    
    if(is.character(bins)){
      bins <- NULL
    }
      
    for(i in 1:nfc){
      
      p <- probability_table[[i]]
      result[[i]] <- suppressWarnings(reliabilitydiag::as.reliabilitydiag(p$prob,p$obs,xvalues=bins))
      
    }
    
    names(result) <- fcnames
    return(result)
    
  }else{
    
    print("Error! Invalid method. Please select \"classic\" or \"CORP\".")
    return(NULL)
    
  }
  
}
