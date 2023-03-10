
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

# Plots
#quantilePlot()

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
spaghettiPlot(f1[,-c(1:2)],xlim=100)

plotFc(fc_obs_data,1,"fanchart",xmax=300)
plotFc(fc_obs_data,1,"spaghetti",xmax=300)

plot(fc_obs_data$forecasts[[1]]$m001,type="l")
plot(fc_obs_data$obs$obs,type="l")

eventDetect(fc_obs_data,)