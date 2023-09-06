#' Report generation function
#'
#' @param data Data set in the required format.
#' @param dest Destination for the compiled HTML report.
#' @return Currently nothing.
#' @export
forecastEvaluationReport <- function(data,dest="./",delete_source=T,
                                     event_change=0.1,event_window=1,contingency_threshold=0.2){
  
  # Filenames
  output_filename <- paste0(dest,"forecast_evaluation_report.html")
  rmd_name <- paste0(dest,"rmd_",as.numeric(Sys.time()),".Rmd")
  
  # Local writing function
  addline <- function(x){cat(paste0(x,"\n"),file=rmd_name,append=T)}
  BR <- function(){cat("\n",file=rmd_name,append=T)}
  addchunk <- function(x,echo=T,include=T,message=T,results='show',fig.keep='all'){
    addline(paste0("```{r, echo=",echo,", include=",include,", message=",message,"}"))
    addline(x)
    addline("```\n")
  }
  
  # -------------------------- #
  # ----- REPORT CONTENT ----- #
  # -------------------------- #
  
  # Title (static now, customizable later)
  addline("---")
  addline("title: \"IEA Forecast Evaluation\"")
  addline("author: \"Recommended Practice R Package\"")
  addline("date: \"`r format(Sys.time(), '%d %B, %Y')`\"")
  addline("output: html_document")
  addline("---")
  
  # Fundamentals
  Nfcc <- length(data$forecasts)
  fcnames <- names(data$forecasts)
  
  # Summary statistics
  addline("## Summary Statistics")
  addchunk("summaryStats(data)",echo = F)
  
  # Visualized forecasts
  addline("## Visualized Forecasts")
  addchunk("
  ti <- 1:100
  for(i in 1:Nfcc){
    f <- data$forecasts[[i]]
    
    if(dim(f)[1]<length(ti)){
      ti <- 1:dim(f)[1]
    }
    
    quantilePlot(Filter(is.numeric,f)[ti,],x=f$TimeStamp[ti],main=fcnames[i],ylab = \"Quantity\")
    lines(data$observations$TimeStamp,data$observations$obs)
  }",echo=F)
  
  # CRPS
  addline("## CRPS - Basic")
  addchunk("forecastEvaluation(data,by_lead_time = F)",echo=F)
  addline("## CRPS - By Lead Time")
  addchunk("forecastEvaluation(data,by_lead_time = T)",echo=F,message=F)
  
  # EVENT DETECTION RELATED MATTERS

  # Restrict data to intersecting timestamps only
  data_eval <- evaluationSet(data)
  
  # Compute event detection tables for all forecast series (NB: takes time at the moment!)
  detect_table_list <- eventDetectionTable(data_eval,
                                           change=event_change,
                                           window=event_window)
  
  # Make contingency tables
  contingency_table <- contingencyTableList(detect_table_list,threshold = contingency_threshold)
  
  # Print contingency table
  addline("## Contingency Table")
  addline(paste0("Change = ",event_change))
  BR()
  addline(paste0("Window = ",event_window))
  BR()
  addline(paste0("Threshold = ",contingency_threshold))
  BR()
  addchunk("printContingencyTable(contingency_table)",echo=F)
  
  # ROC curves
  addline("## ROC Curve")
  addline("```{r, echo=F,message=F,results='hide'}")
  addline("rocCurveList(detect_table_list)")
  addline("```\n")
  
  #addchunk("rocCurveList(detect_table_list)",echo=F,message=F)
  
  # Brier scores
  addline("## Brier Score")
  probability_table <- probabilityTableList(detect_table_list)
  addchunk("brierScoreList(probability_table,prob=T)",echo=F)
  
  # Reliability diagrams
  addline("## Reliability Diagram")
  addchunk("reliabilityDiagramList(probability_table,prob=T)",echo=F,message=F)
  
  # -------------------------- #
  # ----- RENDER REPORT ------ #
  # -------------------------- #
  
  rmarkdown::render(input = rmd_name, 
    #output_format = rmarkdown::html_document(
    #  theme = NULL,
    #  mathjax = NULL,
    #  highlight = NULL
    #),
    output_file = output_filename
  )
  
  if(delete_source){file.remove(rmd_name)}
  
}
