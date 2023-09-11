# INSTALL R PACKAGE
setwd("~/Documents/GitHub/RP-RES-forecast-evaluation/")

library(devtools)
library(roxygen2)

# Update package documentation
document(pkg = ".")

# Install from local repository
install(".")

# ======================================= #
# -------- LOAD PACKAGE AND DATA -------- #
# ======================================= #

# Load Package
library(IEAwind51RP)

# Load data
data("IEAW51-ReadingExample-small") # Swap with appropriate dataset
fc_obs_data <- sample_may23_small # Swap sample_may23_small with appropriate name
rm(sample_may23_small) # Swap sample_may23_small with appropriate name

# ======================================= #
# -------- GENERATE FULL REPORT --------- #
# ======================================= #

# Generate report, with the first argument being the dataset in the correct format
# The other arguments are:
# dest: Destination for the report
# delete_source: By default, the intermediate .Rmd source file is deleted
# event_change: Flat change in the same units as the input data
# event_window: Duration of time window within which the event must occur, always measured in hours
# contingency_threshold: Fraction of ensemble members needed to detect an event

forecastEvaluationReport(fc_obs_data,dest="~/Desktop/",delete_source = T,
                         event_change = -0.1,event_window = 12,contingency_threshold = 0.2)

# ======================================= #
# -------- STEP-BY-STEP EXECUTION ------- #
# ======================================= #

# Summary statistics
summaryStats(fc_obs_data)

# Plot - mostly flexible for experienced R-users for now
# If the "margin" error occurs when trying to plot, then
# execute dev.off() until all plots are gone and try again.

# Assisting variables for plots
fcnames <- names(fc_obs_data$forecasts) # Names of forecast candidates
nfc <- length(fcnames) # Number of competing forecast candidates

# Max first 100 time points
ti <- 1:100
for(i in 1:nfc){
  f <- fc_obs_data$forecasts[[i]]
  
  if(dim(f)[1]<length(ti)){
    ti <- 1:dim(f)[1]
  }
  
  quantilePlot(Filter(is.numeric,f)[ti,],x=f$TimeStamp[ti],main=fcnames[i],ylab = "Quantity")
  lines(fc_obs_data$observations$TimeStamp,fc_obs_data$observations$obs)
}

# Full time series - will take long if it is a big dataset
for(i in 1:nfc){
  f <- fc_obs_data$forecasts[[i]]
  
  ti <- 1:dim(f)[1]
  
  quantilePlot(Filter(is.numeric,f)[ti,],x=f$TimeStamp[ti],main=fcnames[i],ylab = "Quantity")
  lines(fc_obs_data$observations$TimeStamp,fc_obs_data$observations$obs)
}

# ======================================= #
# --------- FORECAST EVALUATION --------- #
# ======================================= #

# CRPS etc.
forecastEvaluation(fc_obs_data,by_lead_time = F)
forecastEvaluation(fc_obs_data,by_lead_time = T)

# Restrict data to intersecting timestamps only
fc_obs_data_eval <- evaluationSet(fc_obs_data)

# Compute event detection tables for all forecast series (NB: takes time at the moment!)
detect_table_list <- eventDetectionTable(fc_obs_data_eval,change=-0.01,window=6)

# Make contingency tables
contingency_table <- contingencyTableList(detect_table_list,threshold = 0.2)

# Print contingency table
printContingencyTable(contingency_table)

# ROC curves
rocCurveList(detect_table_list)

# Brier scores
brierScoreList(detect_table_list)

# Reliability diagrams
reliabilityDiagramList(detect_table_list)

# Reliability diagrams with custom bin selection
reliabilityDiagramList(detect_table_list,bins=seq(0.05,0.95,by=0.1))

# Generate report
forecastEvaluationReport(fc_obs_data,dest="~/Desktop/",delete_source = T,
                         event_change = -0.1,event_window = 12,contingency_threshold = 0.2)
