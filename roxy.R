
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
plot(f1$m001,type="l")
plot(fc_obs_data$obs$obs,type="l")
quantilePlot(f1[1:100,-c(1:2)],x=f1$TimeStamp[1:100])
spaghettiPlot(f1[1:100,-c(1:2)])

plotFc(f1[1:200,-c(1:2)])
plotFc(f1[1:200,-c(1:2)],type = "Spaghetti")

plotFc(f1[1:200,-c(1:2)],type = "Spaghetti",
       x=f1$TimeStamp[1:200],
       observations = fc_obs_data$obs$obs[1:200])

forecastEvaluation(fc_obs_data)

# ======================================= #
# ------- EXECUTION (NEW VERSION) ------- #
# ======================================= #

# Restrict data to intersecting timestamps only
fc_obs_data_eval <- evaluationSet(fc_obs_data)

# Compute event detection tables for all forecast series (NB: takes time at the moment!)
detect_table_list <- eventDetectionTable(fc_obs_data_eval)

# Make contingency tables
contingency_table <- contingencyTableList(detect_table_list)

# Print contingency table
printContingencyTable(contingency_table)

# ROC curve
f=1
roc <- rocCurve(detect_table_list[[f]],main=names(detect_table_list)[[f]])
