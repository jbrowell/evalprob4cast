#' Load all forecast and observation data
#' 
#' @param dir A directory containing forecast and observation data.
#' @return A list with two elements. The first element contains forecast data as a list, and the second contains observations.
#' @export
loadData <- function(path,timeformat_f="default",timeformat_obs="default",
                     timezone_f="UTC",timezone_obs="UTC"){
  
  filelist <- list.files(path)
  fcfiles <- grepl("forecast",filelist)
  nfcfiles <- sum(fcfiles)
  fcnames <- filelist[fcfiles]
  
  datl <- list()
  for(i in 1:nfcfiles){
    datl[[i]] <- read.table(paste0(path,fcnames[i]),sep=",",header=T,stringsAsFactors = F,na.strings = "-")
    names(datl)[i] <- gsub(".csv","",gsub("forecast_","",fcnames[i]))
  }
  
  obs <- read.table(paste0(path,filelist[grepl("observations",filelist)]),sep=",",header=T,stringsAsFactors = F,na.strings = "-")
  colnames(obs)[2] <- "obs"
  
  
  # TimeStamps and TimeZone for observations
  if(timeformat_obs=="NA"){
    obs <- cbind(1:length(obs),obs)
    colnames(obs)[1] <- "TimeStamp"
  }else{
    if(timeformat_obs=="default"){
      timeformat_obs="%Y-%m-%d %H:%M"
    }
    obs[,1] <- as.POSIXct(strptime(obs[,1],format=timeformat_obs,tz=timezone_obs))
    colnames(obs)[1] <- "TimeStamp"
  }
  
  # TimeStamps and TimeZone for forecasts
  if(timeformat_f=="NA"){
    for(i in 1:nfcfiles){
      datl[[i]] <- cbind(1:N,datl[[i]])
      colnames(datl[[i]])[1] <- "TimeStamp"
    }
  }else{
    if(timeformat_f=="default"){
      timeformat_f="%Y-%m-%d %H:%M"
    }
    for(i in 1:nfcfiles){
      datl[[i]][,1] <- as.POSIXct(strptime(datl[[i]][,1],format=timeformat_f,tz=timezone_f))
      colnames(datl[[i]])[1] <- "TimeStamp"
      if(is.null(datl[[i]]$BaseTime)){
        datl[[i]] <- cbind(data.frame(BaseTime = datl[[i]]$TimeStamp-lubridate::hour(datl[[i]]$TimeStamp)*60^2),
                           datl[[i]])
        datl[[i]]
      }
    }
  }
  
  return(list(forecasts=datl,
              observations=obs))
  
}
