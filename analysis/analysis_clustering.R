
#creating a logfile where all output will go
sink(file = paste0(log_folder, date, "_", project_name, "_clustering_log.txt"), split = TRUE)
#checks folders for presence of clustering results from previous runs
#checks whether analysis column in meta contains "0" or "2" (ad-hoc removal or addition of samples after the first round of clustering)
#sets clustering mode accordingly
set_clustering_mode()

#restores previous clustering, if available
#reuses available clustering (reuses mapping from FlowSOM or does KNN classification with automatic parameter tuning) if clustering available and new samples were added
#(if analysis column in meta contains "2")
#does clustering with selected clustering engine if above is not true
do_clustering()

#produces plots for cluster characterization and diagnostics
do_clustering_diagnostics()

#asks for confirmation to continue if in interactive mode and first_run_mode is on (value of "1") in settings
#reclusters data with new parameters if user is not satisfied with results
continue_or_recluster()


merge_and_annotate()

merge_exprs_and_clusters() #WIP!!!!!!!!


sink()

