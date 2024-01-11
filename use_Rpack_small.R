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

# ======================================== #
# ----- STANDARD FORECAST EVALUATION ----- #
# ======================================== #

# CRPS etc.
forecastEvaluation(fc_obs_data,by_lead_time = F)
(crps_by_leadtime <- forecastEvaluation(fc_obs_data,by_lead_time = T))
plotScoreByLeadtime(crps_by_leadtime)
plotScoreByLeadtime(crps_by_leadtime,ylim=c(0,0.2),main="CRPS by leadtime") # Customizable

# Restrict data to intersecting timestamps only
fc_obs_data_eval <- evaluationSet(fc_obs_data)

# Individual forecast candidates and the observation set
f1 <- as.matrix(fc_obs_data_eval$forecasts$pred_power_northwales[,-c(1)])
f2 <- as.matrix(fc_obs_data_eval$forecasts$pred_power_midwales[,-c(1)])
f3 <- as.matrix(fc_obs_data_eval$forecasts$pred_power_lanarkshire[,-c(1)])
y <- fc_obs_data_eval$observations$obs

# Rank histograms with (default) m+1 bins
rankHistogram(f1,y)
rankHistogram(f2,y)
rankHistogram(f3,y)

# ... with 10 bins
rankHistogram(f1,y,nbins = 10)
rankHistogram(f2,y,nbins = 10)
rankHistogram(f3,y,nbins = 10)

# ... or varying bins for e.g. the first forecast candidate
rankHistogram(f1,y,nbins = 10)
rankHistogram(f1,y,nbins = 20)
rankHistogram(f1,y,nbins = 40)
rankHistogram(f1,y,nbins = 500)

# ========================================= #
# ---- EVENT-BASED FORECAST EVALUATION ---- #
# ========================================= #

# Compute event detection tables for all forecast series, can take some time
# Event detection tables - select change or range, not both! Here it is range...
detect_table_list <- eventDetectionTable(fc_obs_data_eval,range=c(0.5,1),window=6)
plot(fc_obs_data_eval$forecasts$pred_power_northwales$w3,type="l")
lines(detect_table_list$pred_power_northwales$w3,type="l",col="red",lty=2)

# And here, event defined as a change...
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

# Classic reliability diagrams (default)
reliabilityDiagramList(detect_table_list)
# Classic reliability diagrams (custom bins)
reliabilityDiagramList(detect_table_list,bins = seq(0.04,0.96,by=0.04))

# CORP-based reliability diagrams
reliabilityDiagramList(detect_table_list,method = "CORP")
# CORP-based reliability diagrams with custom bin selection
reliabilityDiagramList(detect_table_list,method = "CORP",bins = seq(0.05,0.95,by=0.1))

# As expected, Lanarkshire forecasts are by far the worst at predicting events 
# from North Wales, both according to Brier score and Reliability diagram

# ======================================== #
# ------------ OTHER FEATURES ------------ #
# ======================================== #

# It is possible to export and import detection tables in the following way:

# Export detection tables by setting the export.results flag to TRUE:
detect_table_list <- eventDetectionTable(fc_obs_data_eval,change=-0.01,window=6,export.results = T)

# The tables can be re-imported by specifying their path, in this case the results folder:
imported_table_list <- importDetectionTable("./results")

# Check that the exported and imported are identical:
setequal(detect_table_list,imported_table_list)
