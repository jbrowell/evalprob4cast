#' Load all forecast and observation data
#' 
#' @param dir A directory containing forecast and observation data.
#' @return A list with two elements. The first element contains forecast data as a list, and the second contains observations.
#' @export
loadData <- function(dir){

  filelist <- list.files(dir)
  fcfiles <- grepl("forecast",filelist)
  nfcfiles <- sum(fcfiles)
  fcnames <- filelist[fcfiles]
  
  datl <- list()
  for(i in 1:nfcfiles){
    datl[[i]] <- read.table(paste0("data/",fcnames[i]),sep=",",header=T,stringsAsFactors = F,na.strings = "-")
  }
  
  obs <- read.table(paste0("data/",filelist[grepl("observations",filelist)]),sep=",",header=T,stringsAsFactors = F,na.strings = "-")
  colnames(obs)[2] <- "obs"
  
  return(list(forecasts=datl,
              observations=obs))
    
}