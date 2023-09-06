#' DO NOT USE THIS YET
#'
#' @param data Data set in the required format.
#' @param dest Destination for the compiled HTML report.
#' @return Currently nothing.
#' @export
forecastEvaluationReport <- function(data,dest="./",options=NULL) {
  
  # Filenames
  output_filename <- paste0(dest,"forecast_evaluation_report.html")
  rmd_name <- paste0(dest,"rmd_",as.numeric(Sys.time()),".R")
  
  # -------------------------- #
  # ----- REPORT CONTENT ----- #
  # -------------------------- #
  
  # Summary statistics
  cat("#' ## Summary statistics\n",file = rmd_name,append = T)
  cat("summaryStats(data)\n",file = rmd_name,append = T)
  
  # CRPS
  cat("#' ## CRPS\n",file = rmd_name,append = T)
  cat("#' ### CRPS \"blind\"\n",file = rmd_name,append = T)
  cat("forecastEvaluation(data,by_lead_time = F)\n",file = rmd_name,append = T)
  cat("#' ### CRPS by lead time\n",file = rmd_name,append = T)
  cat("forecastEvaluation(data,by_lead_time = T)\n",file = rmd_name,append = T)
  
  
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
  
}
