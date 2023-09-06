# INSTALL R PACKAGE
setwd("~/Documents/GitHub/RP-RES-forecast-evaluation/")

library(devtools)
library(roxygen2)

# Update package documentation
document(pkg = ".")

# Install from local repository
install(".")

# ======================================= #
# ------------ INITIAL STUFF ------------ #
# ======================================= #

# Load Package
library(IEAwind51RP)

# Load data
data("IEAW51-ReadingExample-small")
fc_obs_data <- sample_may23_small
rm(sample_may23_small)

# Summary statistics
summaryStats(fc_obs_data)

# Plot
ti <- 1:100
for(i in 1:3){
  f <- fc_obs_data$forecasts[[i]]
  quantilePlot(f[ti,-c(1:2)],x=f$TimeStamp[ti],main=paste0("Forecast ",LETTERS[i]),ylab = "Normalized Wind Power",ylim=c(0,1))
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
names(detect_table_list) <- c("Forecast A","Forecast B","Forecast C")

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

# As expected, Lanarkshire forecasts are by far the worst at predicting events 
# from North Wales, both according to Brier score and Reliability diagram