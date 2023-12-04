#Cytomata is an open-source script collection that integrates tools and methods used for mass cytometry (CyTOF) analysis into a semi-automated pipeline.
#Author: Lev Petrov

#FILE STRUCTURE ################
#Cytomata
#holds scripts, distributed through github

#Cytomata_data
#holds both input and output files
#Cytomata_data/<project_name>/fcs/


#PROJECT SETUP  ################
#one only thing you MUST change in the script before running it is this:
path_to_cytomata <- "~/DOCTORATE/Cytomata/"
#all the other settings are to be changed using following:
#setting.xlsx in Cytomata folder (general project setup, normalization, clustering, UMAP settings)
#Cytomata_data/<project_folder>/meta/meta.xlsx (sample stratification, sample addition and removal for reanalyzis)
#Cytomata_data/<project_folder>/normalization/<anchor_id>/normalization_settings.csv (adjusting automatic normalization settings, appears automatically during normalization)
#Cytomata_data/<project_folder>/meta/subset_feature_selection.xlsx (selecting features for analysis for each subset, order will be preserved in plots)
#Cytomata/analysis/analysis_plot_settings.R (color palletes, global text scaling, general themes for each plot type)
#Cytomata/analysis/clustering/cluster_annotation.xlsx (manually annotate and merge clusters, appears automatically during clustering)

#ENVIRONMENT SETUP ################
setwd(path_to_cytomata)
source("general_functions.R")
setwd(path_to_cytomata)
source("environment_setup.R")
set.seed(1234)

settings <- parse_settings()
path_to_data_folder <- settings$value[settings$setting == "path_to_data_folder"]
project_name <- settings$value[settings$setting == "project_name"]
# path_to_cytomata <- "C:/Users/feder/Desktop/Charite/Cytomata"
# path_to_data_folder <- "C:/Users/feder/Desktop/Charite/Cytomata/Cytomata_data/"
# project_name <- "dev_database"



#this script prepares folder structure for the new project
setwd(path_to_cytomata)
source("folder_manager.R")

#set filter to select metafile
#metafile MUST have following columns: "id", "fcs", "batch", "analysis" and "group". script supports having multiple grouping-columns (e.g. "group_2", "group_3" etc.)
#analysis column defines whether sample should be included in final analysis. duplicates of anchor/reference files are filtered out by default during analysis
#analysis column also accepts values of 0 for samples to be dropped from final analysis or 2 for samples added after the main analysis run (if previous clustering needs to be preserved)
meta <- load_metafile(meta_naming_scheme = settings$value[settings$setting == "meta_naming_scheme"])
#samples that will be dropped from analysis have to be assigned "drop" in the respective "group" column
#extracting marker panel. check "feature" variable and set it to 0 for unused channels.
#common pre-processing channels are automatically set to 0
panel <- load_panel()
feature_markers <- panel$antigen[panel$feature == 1]

#CLEANING AND DEBARCODING  ################
#sadly, this part is best done in a cloud-based solution like OMIQ or Cytobank due to the massive data and the fact that gating is still best done manually
#upload your raw data, remove calibration beads, gate on DNA channels and assign barcode identities
#also a good place to MAKE SURE ALL CHANNELS HAVE THE SAME NAME
#export and put the files into Cytomata_data/<project_name>/fcs/2_debarcoded/. folder


#Lev's unbiased batch adjustment (LUBA)  ################
#automatically chooses the best anchor out of available, supports multi-step(anchor) adjustment
#automatically chooses optimal percentile via peak recognition and diversity metric estimation
anchor_ids <- unlist(strsplit(settings$value[settings$setting == "anchor_ids"], split = ", ", fixed = TRUE))

if (settings$value[settings$setting == "do_normalization"] == 1) {
    #anchor = technical replicate included with each batch. Can be one or multiple. If multiple, normalization is done in order from left to right
    #files are saved to fcs/3_normalized/<anchor_id>
    #if TRUE, optimal anchor is automatically selected individually for each channel
    #if FLASE, optimal anchor is selected globally, for all channels - NOT IMPLEMENTED YET!
    find_anchor_by_channel <- TRUE
    setwd(path_to_cytomata)
    source("./normalization/normalization_master.R")
}




#FULL DATA MEAN AND SD CALCULATION FOR SCALING  ################
if (settings$value[settings$setting == "do_normalization"] == 1) {
    setwd(path_to_cytomata)
    source("database_injection.R")
}

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

if (settings$value[settings$setting == "do_analysis"] == 1) {
    setwd(path_to_cytomata)
    source("./analysis/analysis_master.R")
}

#POST-PROCESSING ADD-ONS  ################
#population of interest-specific analyses like e.g. activated T cell flagging

#Need post-factum sample addition feature for: 
#normalization (could be okay, choosing a new main anchor can be argumented for)
#KNN clustering
#for this, a nice metric is available:
#LOCAL INVERSE SIMPSON's INDEX
#how many different clusters are in the local neighbourhood of a cell, inspired by
#https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009071