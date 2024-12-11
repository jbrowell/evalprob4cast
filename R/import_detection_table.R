#' Import detection tables.
#'
#' @param path the path to the directory where the files are to be imported from. 
#' All files will be imported, so the directory must contain only exactly the files that are to be imported, nothing more, nothing less.
#' @param tz the timezone of the TimeStamp column in the files.
#' @return a list of all the imported tables.
#' @export

import_detection_table <- function(path,tz="UTC"){
  
  files <- list.files(path)
  n <- length(files)
  
  if(n == 0){
    print("No files found at this path!\n")
    return(-1)
  }
  
  result <- list()
  for(i in 1:n){
    x <- read.table(paste0(path,"/",files[i]),sep=",",header=T,stringsAsFactors = F)
    x$TimeStamp <- as.POSIXct(x$TimeStamp,tz=tz)
    if("BaseTime" %in% names(x)){
      x$BaseTime <- as.POSIXct(x$BaseTime,tz=tz)
    }
    result[[i]] <- x
    names(result)[i] <- gsub("detect_table","",gsub(".csv","",files[i]))
  }
  
  return(result)
  
}
