
setwd("~/Documents/GitHub/RP-RES-forecast-evaluation/")

library(devtools)
library(roxygen2)

# Update package documentation
document(pkg = ".")

# Install from local repository
install(".")
library(IEAwind51RP)




## Test functions...

# Load data
fc_obs_data <- loadData("data/")

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
quantilePlot(f1[,-1],xlim=100)
spaghettiPlot(f1[,-1],xlim=100)

plotFc(fc_obs_data,1,"fanchart",xmax=300)
plotFc(fc_obs_data,1,"spaghetti",xmax=300)

quantilePlot(f1[,-1],xlim=500)
spaghettiPlot(f1[,-1],xlim=500)

plot(fc_obs_data$forecasts[[1]]$m001,type="l")
plot(fc_obs_data$obs$obs,type="l")
