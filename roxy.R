
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

forecastEvaluation(fc_obs_data)

#eventDetect(fc_obs_data,win=1)
# ========================= #
# ------- EXECUTION ------- #
# ========================= #

# New to roxy
f <- fc_obs_data$forecasts
obs <- fc_obs_data$observations
nfcfiles <- length(fc_obs_data$forecasts)
fcnames <- names(fc_obs_data$forecasts)

# Restrict data to intersecting timestamps only
ts_intersect <- obs$TimeStamp
for(i in 1:nfcfiles){
  ts_intersect <- intersect(ts_intersect,f[[i]]$TimeStamp)
}
obs_ev <- obs[obs$TimeStamp %in% ts_intersect,]
obs_ev <- obs_ev[!duplicated(obs_ev$TimeStamp),]
f_ev <- list()
for(i in 1:nfcfiles){
  f_ev[[i]] <- f[[i]][f[[i]]$TimeStamp %in% ts_intersect,]
  f_ev[[i]]$BaseTime <- NULL # added
  f_ev[[i]] <- f_ev[[i]][!duplicated(f_ev[[i]]$TimeStamp),]
}

# Compute event detection tables for all forecast series.
for(i in 1:nfcfiles){
  dat_eval <- merge(obs_ev,f_ev[[i]])
  detect_table <- eventDetect(dat_eval,ch=-30,win=4)
  detect_table <- detect_table[!is.na(rowMeans(detect_table[,-1])),]
  
  # Export detection table to results folder
  write.csv(detect_table,paste0("results/detect_table",gsub("forecast","",fcnames[i])),row.names=F)
}

# Make contingency tables
contingency_table <- list()
for(i in 1:nfcfiles){
  
  # Load detection table from results folder
  detect_table <- read.table(paste0("results/detect_table",gsub("forecast","",fcnames[i])),sep=",",header=T)
  
  # Prepare scores for binary classifier
  M_eval <- dim(detect_table)[2]-2
  detect_table_sum <- data.frame(obs=detect_table[,2],
                                 forecast=apply(detect_table[,-c(1,2)],1,function(x){sum(x)/M_eval}))
  PRROC_obj <- roc.curve(scores.class0 = detect_table_sum[,2],weights.class0 = detect_table_sum[,1],curve=T)
  
  # Plot ROC curve
  svg(file=paste0("results/roc_",str_pad(i,2,pad="0"),"r.svg"),width = 6, height = 5)
  plot(PRROC_obj,main=fcnames[i])
  dev.off()
  
  # Get binary table for obs vs. forecast (currently 5 positives needed for a detection)
  detect_table_ct <- detectToBinary(detect_table_sum,threshold=5/M_eval)
  
  # Make contingency table
  contingency_table[[i]] <- contingencyTable(detect_table_ct)
  
}

# Print contingency table
contingency_table_all <- NULL
for(i in 1:nfcfiles){
  
  contingency_table_all <- rbind(contingency_table_all,contingency_table[[i]])
  
}
contingency_table_all$forecast <- fcnames
contingency_table_all <- contingency_table_all[,c(7,1:6)]

cat("CONTINGENCY TABLE\n")
cat("-----------------\n")
print(contingency_table_all)
cat("\n")
