# Set working directory

# Set working directory to package repo directory

# ======================================= #
# ------------ INITIAL STEPS ------------ #
# ======================================= #

# Load Package
library(evalprob4cast)

# Load data
data("IEAW51-MSEPS-DKsynop48")
fc_obs_data <- mseps_dk_thoroe48
rm(mseps_dk_thoroe48)

# Summary statistics
summary_stats(fc_obs_data)

# Plot
plot_observations(fc_obs_data$observations, all=T)
plot_forecasts(fc_obs_data$forecasts$forecasts1)
lines(fc_obs_data$observations, lwd=1)

plot_forecasts(fc_obs_data$forecasts$forecasts2)
lines(fc_obs_data$observations, lwd=1)

plot_forecasts(fc_obs_data$forecasts$forecasts1, by="leadtime")
lines(fc_obs_data$observations, lwd=1)
plot_forecasts(fc_obs_data$forecasts$forecasts1, by="leadtime", lead=12)
lines(fc_obs_data$observations, lwd=1)
plot_forecasts(fc_obs_data$forecasts$forecasts1, by="leadtime", lead=48)
lines(fc_obs_data$observations, lwd=1)

# ======================================== #
# ----- STANDARD FORECAST EVALUATION ----- #
# ======================================== #

# CRPS
evaluate_marginal_distribution(fc_obs_data,by_lead_time = T)
(crps_by_leadtime <- evaluate_marginal_distribution(fc_obs_data,by_lead_time = T))
plot_score_by_leadtime(crps_by_leadtime)

# Restrict data to intersecting timestamps only
fc_obs_data_eval <- make_evaluation_subset(fc_obs_data, lead_time = 6)

# Rank _histograms with (default) m+1 bins
rank_histogram(fc_obs_data_eval$forecasts$forecasts1, fc_obs_data_eval$observations$obs)
rank_histogram(fc_obs_data_eval$forecasts$forecasts2, fc_obs_data_eval$observations$obs)

# ... or the entire thing at once
rank_histogram_list(fc_obs_data_eval)
rank_histogram_list(fc_obs_data_eval,nbins=20)

# ========================================= #
# ---- EVENT-BASED FORECAST EVALUATION ---- #
# ========================================= #

# Compute event detection tables for all forecast series, can take some time
# Event detection tables - select change or range, not both! Here it is change...
detect_table_list <- event_detection_table(fc_obs_data_eval,change=5,window=6)
plot(fc_obs_data_eval$forecasts$forecasts1$m001,type="l")
lines(12*detect_table_list$forecasts1$m001,type="l",col="red",lty=2)

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

# As expected, Lanarkshire forecasts are by far the worst at predicting events 
# from North Wales, both according to Brier score and Reliability diagram

# ======================================== #
# ------------ OTHER FEATURES ------------ #
# ======================================== #

# It is possible to export and import detection tables in the following way:

# Export detection tables by setting the export.results flag to TRUE:
detect_table_list <- event_detection_table(fc_obs_data_eval,change=5,window=6,export.results = T)

# The tables can be re-imported by specifying their path, in this case the results folder:
imported_table_list <- import_detection_table("./results")

# Check that the exported and imported are identical:
setequal(detect_table_list,imported_table_list)

# ======================================== #
# ------------ GENERATE REPORT ----------- #
# ======================================== #

generate_forecast_evaluation_report(fc_obs_data_eval, event_change = 5, event_window = 6)
