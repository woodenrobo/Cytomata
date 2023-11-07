library(flowCore)
library(ggplot2)
library(cowplot)
library(ggridges)
library(progress)
library(dplyr)
library(readxl)
library(provenance)
library(scales)
library(ggnewscale)
#1 means all events are used, this is not used for normalization step itself, only for plotting of densities
sampling_factor <- 0.1
hide_zeroes_in_ridges <- 1
#Kolmogorov-Smirnov can be computed in a pairwise manner or against total channel distribution
#pairwise or total
ks_testing <- "pairwise"


start_time <- Sys.time()
date <- gsub('-', '', strsplit(x = as.character(start_time), split = ' ')[[1]][1])
#creating a logfile where all output will go
sink(file = paste0(log_folder, date, "_", project_name, "_log.txt"), split = TRUE)

setwd(path_to_cytomata)
source("./normalization/normalization_functions.R")
setwd(path_to_cytomata)
source("./normalization/normalization_plots.R")

#automatically chooses the best (most "average") anchor out of available for each channel
#automatically chooses optimal percentile via diversity metric estimation
#creates plots for user to verify automatic settings
#creates a .csv table for user to adjust automatically set parameters (IF IN INTERACTIVE SESSION)
setwd(path_to_cytomata)
source("./normalization/normalization_exploration_adjustment.R")
#plots reduction in variance, dissimilarity metrics, distribution changes
#if anchor file has little events, density can vary a little due to downsampling done for ridgeplots
#quantile values and scaling factor calculations are done on 100 percent of events
setwd(path_to_cytomata)
source("./normalization/normalization_diagnostics.R")

end_time <- Sys.time()

timediff <- round(as.numeric(gsub("Time difference of ", "", difftime(end_time, start_time, units = "mins"))), 2)

cat(paste0(date, " ", project_name, " normalization run ended successfully\n", "Total time elapsed in minutes: ", timediff),"\n")


sink()