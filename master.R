#Cytomata is an open-source script collection that integrates tools and methods used for mass cytometry (CyTOF) analysis into an automated pipeline.
#Author: Lev Petrov

#FILE STRUCTURE ################

#Cytomata
#holds scripts, distributed through github

#Cytomata_data
#holds both input and output files
#Cytomata_data/<project_name>/fcs/

#PROJECT SETUP  ################
path_to_cytomata <- "~/DOCTORATE/Cytomata/"
path_to_data_folder <- "~/DOCTORATE/Cytomata_data/"
project_name <- "dev_project_panel_1"

#ENVIRONMENT SETUP ################
setwd(path_to_cytomata)
source("general_functions.R")
setwd(path_to_cytomata)
#source("environment_setup.R")
set.seed(1234)


#this script prepares folder structure for the new project
setwd(path_to_cytomata)
source("folder_manager.R")

#set filter to select metafile
#metafile MUST have following columns: "id", "fcs", "batch", "analysis" and "group". script supports having multiple grouping-columns (e.g. "group_2", "group_3" etc.)
#analysis column defines whether sample should be included in final analysis. duplicates of anchor/reference files are filtered out by default
meta <- load_metafile(meta_naming_scheme = "meta")
#extracting marker panel. check "feature" variable and set it to 0 for unused channels.
#common pre-processing channels are automatically set to 0
panel <- load_panel()
feature_markers <- panel$antigen[panel$feature == 1]
#CLEANING AND DEBARCODING  ################
#sadly, this part is best done in a cloud-based solution like OMIQ or Cytobank due to the massive data and the fact that gating is still best done manually
#upload your raw data, remove calibration beads, gate on DNA channels and assign barcode identities
#also a good place to make sure that all channels have proper names
#export and put the files into Cytomata_data/<project_name>/fcs/2_debarcoded/. folder


#Lev's unbiased batch adjustment (LUBA)  ################
#automatically chooses the best anchor out of available, supports multi-step(anchor) adjustment
#automatically chooses optimal percentile via peak recognition and diversity metric estimation

#anchor = technical replicate included with each batch. Can be one or multiple. If multiple, normalization is done in order from left to right
#files are saved to fcs/3_normalized/<anchor_id>
anchor_ids <- c("HC-4", "HC-100")
#if TRUE, optimal anchor is automatically selected individually for each channel
#if FLASE, optimal anchor is selected globally, for all channels - NOT IMPLEMENTED PROPERLY YET!
find_anchor_by_channel <- TRUE
setwd(path_to_cytomata)
source("./normalization/normalization_master.R")



#FULL DATA MEAN AND SD CALCULATION FOR SCALING  ################
setwd(path_to_cytomata)
#database_injection.R
#creates a database to facilitate calculating statistics over tens of millions of cells at a time


#SUBSETTING OF POPULATIONS OF INTEREST  ################
#sadly, this part is best done in a cloud-based solution like OMIQ or Cytobank due to the massive data and the fact that gating is still best done manually
#upload your normalized data and use a gating strategy based on your panel design to separate the cells into populations of interest
#export resulting files and put them into Cytomata_data/<project_name>/fcs/4_subsets/. folder


#FINAL PROCESSING AND EXPLORATION  ################
#data structure exploration
#clustering, automatic cluster merging, clustering QC, annotation facilitation
#signal intensity analysis
#automatic testing of all conditions included in the metafile


#POST-PROCESSING ADD-ONS  ################
#population of interest-specific analyses like e.g. activated T cell flagging

#Need post-factum sample addition feature for: 
#normalization (could be okay, choosing a new main anchor can be argumented for)
#KNN clustering
#for this, a nice metric is available:
#LOCAL INVERSE SIMPSON's INDEX
#how many different clusters are in the local neighbourhood of a cell, inspired by
#https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009071