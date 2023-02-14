
library(devtools)
library(roxygen2)

# Update package documentation
document(pkg = ".")

# Install from local repository
install(".")
library(IEAwind51RP)




## Test functions...

fc_obs_data <- loadData("data/")

plot(fc_obs_data$forecasts[[1]]$m001,type="l")
plot(fc_obs_data$obs$obs,type="l")
