#rm(list=ls())
#cat("\014")
setwd("~/Documents/GitHub/RP-RES-forecast-evaluation/")
#setwd("~/GitHub/RP-RES-forecast-evaluation/")

library(devtools)
library(roxygen2)

# Update package documentation
document(pkg = ".")

# Install from local repository
install(".")
library(IEAwind51RP)

## Test functions...

## Load data
# fc_obs_data <- loadData("sysdata/")
data("IEAW51-SampleData")
fc_obs_data <- sample_fc_obs_data
rm(sample_fc_obs_data)

data("IEAW51-ReadingExample")
fc_obs_data <- sample_wales
rm(sample_wales)
colnames(fc_obs_data$forecasts$pred_power_northwales)[1:2] <- c("TimeStamp","BaseTime")
colnames(fc_obs_data$forecasts$pred_power_midwales)[1:2] <- c("TimeStamp","BaseTime")
colnames(fc_obs_data$observations) <- c("TimeStamp","obs")

# Print summary statistics
summaryStats(fc_obs_data)

# Documentation
?loadData
?summaryStats
?quantilePlot
?spaghettiPlot

# Other tests
par(mfrow=c(1,1))

f1 <- fc_obs_data$forecasts[[1]]
plot(f1[,min(which(sapply(f1,class)=="numeric"))],type="l") # Plot first ensemble member
plot(fc_obs_data$observations,type="l")

quantilePlot(f1[1:100,-c(1:2)])
spaghettiPlot(f1[1:100,-c(1:2)])

quantilePlot(f1[1:100,-c(1:2)],x=f1$TimeStamp[1:100])
lines(fc_obs_data$observations$dtm,fc_obs_data$observations$npower)

f2 <- fc_obs_data$forecasts[[2]]
quantilePlot(f2[1:100,-c(1:2)],x=f2$TimeStamp[1:100])
lines(fc_obs_data$observations$dtm,fc_obs_data$observations$npower)


# ======================================= #
# --------- FORECAST EVALUATION --------- #
# ======================================= #

# CRPS etc.
forecastEvaluation(fc_obs_data,by_lead_time = F)
forecastEvaluation(fc_obs_data,by_lead_time = T)

# Restrict data to intersecting timestamps only
fc_obs_data_eval <- evaluationSet(fc_obs_data)

# Compute event detection tables for all forecast series (NB: takes time at the moment!)
detect_table_list <- eventDetectionTable(fc_obs_data_eval,change=-0.03,window=6)

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

# Try reporting
forecastEvaluationReport(fc_obs_data,dest="~/Desktop/",delete_source = T)