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
summary_stats(fc_obs_data)

# Plot
plot_observations(fc_obs_data$observations)
plot_observations(fc_obs_data$observations, all=T)
plot_forecasts(fc_obs_data$forecasts$pred_power_northwales)
lines(fc_obs_data$observations, lwd=1)

plot_forecasts(fc_obs_data$forecasts$pred_power_midwales)
plot_forecasts(fc_obs_data$forecasts$pred_power_lanarkshire)
plot_forecasts(fc_obs_data$forecasts$pred_power_northwales, all = T)
plot_forecasts(fc_obs_data$forecasts$pred_power_midwales, all = T)
plot_forecasts(fc_obs_data$forecasts$pred_power_lanarkshire, all = T)

# ======================================== #
# ----- STANDARD FORECAST EVALUATION ----- #
# ======================================== #

# CRPS etc.
evaluate_marginal_distribution(fc_obs_data,by_lead_time = F)
(crps_by_leadtime <- evaluate_marginal_distribution(fc_obs_data,by_lead_time = T))
plot_score_by_leadtime(crps_by_leadtime)
plot_score_by_leadtime(crps_by_leadtime, ylim=c(0,0.2), main="CRPS by leadtime") # Customizable

# Restrict data to intersecting timestamps only
fc_obs_data_eval <- make_evaluation_subset(fc_obs_data)

# Individual forecast candidates and the observation set
f1 <- as.matrix(fc_obs_data_eval$forecasts$pred_power_northwales[,-c(1)])
f2 <- as.matrix(fc_obs_data_eval$forecasts$pred_power_midwales[,-c(1)])
f3 <- as.matrix(fc_obs_data_eval$forecasts$pred_power_lanarkshire[,-c(1)])
y <- fc_obs_data_eval$observations$obs

# Rank _histograms with (default) m+1 bins
rank_histogram(f1,y)
rank_histogram(f2,y)
rank_histogram(f3,y)

# ... with 10 bins
rank_histogram(f1,y,nbins = 10)
rank_histogram(f2,y,nbins = 10)
rank_histogram(f3,y,nbins = 10)

# ... or varying bins for e.g. the first forecast candidate
rank_histogram(f1,y,nbins = 10)
rank_histogram(f1,y,nbins = 20)
rank_histogram(f1,y,nbins = 40)
rank_histogram(f1,y,nbins = 500)

# ... or the entire thing at once
rank_histogram_list(fc_obs_data_eval)
rank_histogram_list(fc_obs_data_eval,nbins=20)

# ========================================= #
# ---- EVENT-BASED FORECAST EVALUATION ---- #
# ========================================= #

# Compute event detection tables for all forecast series, can take some time
# Event detection tables - select change or range, not both! Here it is range...
detect_table_list <- event_detection_table(fc_obs_data_eval,range=c(0.5,1),window=6)
plot(fc_obs_data_eval$forecasts$pred_power_northwales$w3,type="l")
lines(detect_table_list$pred_power_northwales$w3,type="l",col="red",lty=2)

# And here, event defined as a change...
detect_table_list <- event_detection_table(fc_obs_data_eval,change=-0.01,window=6)
names(detect_table_list) <- c("Forecast A","Forecast B","Forecast C")

# Make contingency tables
ct_tab <- contingency_table_list(detect_table_list,threshold = 0.2)

# Print contingency table
print_contingency_table(ct_tab)

# ROC curves
roc_curve_list(detect_table_list)

# Brier scores
brier_score_list(detect_table_list)

# Classic reliability diagrams (default)
reliability_diagram_list(detect_table_list)
# Classic reliability diagrams (custom bins)
reliability_diagram_list(detect_table_list,bins = seq(0.04,0.96,by=0.04))

# CORP-based reliability diagrams
reliability_diagram_list(detect_table_list,method = "CORP")
# CORP-based reliability diagrams with custom bin selection
reliability_diagram_list(detect_table_list,method = "CORP",bins = seq(0.05,0.95,by=0.1))

# As expected, Lanarkshire forecasts are by far the worst at predicting events 
# from North Wales, both according to Brier score and Reliability diagram

# ======================================== #
# ------------ OTHER FEATURES ------------ #
# ======================================== #

# It is possible to export and import detection tables in the following way:

# Export detection tables by setting the export.results flag to TRUE:
detect_table_list <- event_detection_table(fc_obs_data_eval,change=-0.01,window=6,export.results = T)

# The tables can be re-imported by specifying their path, in this case the results folder:
imported_table_list <- import_detection_table("./results")

# Check that the exported and imported are identical:
setequal(detect_table_list,imported_table_list)

# ======================================== #
# ------------ GENERATE REPORT ----------- #
# ======================================== #

generate_forecast_evaluation_report(fc_obs_data, event_change = -0.01, event_window = 6)
