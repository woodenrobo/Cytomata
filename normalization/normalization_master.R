library(flowCore)
library(ggplot2)
library(cowplot)
library(ggridges)
library(progress)
library(dplyr)
library(readxl)



#1 means all events are used, this is not used for normalization step itself, only for exploration and diagnostics
downsampling_factor <- 0.5




start_time <- Sys.time()
date <- gsub('-','',strsplit(x = as.character(start_time), split = ' ')[[1]][1])
#creating a logfile where all output will go
sink(file=paste0(log_folder, date, "_", project_name, "_log.txt"), split=TRUE)




#automatically chooses the best anchor out of available
#automatically chooses optimal percentile via peak recognition and diversity metric estimation
#creates plots for user to verify automatic settings
#creates a settings .csv table for user to adjust automatic settings
source("normalization_exploration.R")

#LOCAL INVERSE SIMPSON's INDEX
#how many different batches are in the local neighbourhood of a cell, inspired by
#https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009071




end_time <- Sys.time()

timediff <- round(as.numeric(gsub("Time difference of ", "", difftime(end_time, start_time, units = "mins"))), 2)

cat(paste0(date, " ", project_name, " normalization run ended successfully\n", "Total time elapsed in minutes: ", timediff),"\n")


sink()