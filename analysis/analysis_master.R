#APPLYING SETTINGS ######
first_run_mode <- as.numeric(settings$value[settings$setting == "first_run_mode"])
grouping_columns <- unlist(strsplit(settings$value[settings$setting == "grouping_columns"], split = ", ", fixed = TRUE))
grouping_orders <- unlist(strsplit(settings$value[settings$setting == "grouping_orders"], split = "; ", fixed = TRUE))
data_subsets <- unlist(strsplit(settings$value[settings$setting == "data_subsets"], split = ", ", fixed = TRUE))
sampling_factors <- as.numeric(unlist(strsplit(settings$value[settings$setting == "sampling"], split = ", ", fixed = TRUE)))
event_cutoff <- as.numeric(settings$value[settings$setting == "event_cutoff"])
low_var_feature_removal <- as.numeric(unlist(strsplit(settings$value[settings$setting == "low_var_feature_removal"], split = ", ", fixed = TRUE)))



if (length(low_var_feature_removal) > 1 && low_var_feature_removal[1] == 1) {
    top_var_features <- low_var_feature_removal[2]
}
if (length(low_var_feature_removal) == 1 && low_var_feature_removal[1] == 1) {
    top_var_features <- 20
}
clustering_engine <- settings$value[settings$setting == "clustering_engine"]
clustering_k <- as.numeric(unlist(strsplit(settings$value[settings$setting == "clustering_k"], split = ", ", fixed = TRUE)))
fs_n_dims <- as.numeric(settings$value[settings$setting == "fs_n_dims"])
ccp_delta_cutoff <- as.numeric(settings$value[settings$setting == "ccp_delta_cutoff"])
umap_n <- as.numeric(unlist(strsplit(settings$value[settings$setting == "umap_n"], split = ", ", fixed = TRUE)))
umap_min_dist <- as.numeric(unlist(strsplit(settings$value[settings$setting == "umap_min_dist"], split = ", ", fixed = TRUE)))

start_time <- Sys.time()
date <- gsub('-', '', strsplit(x = as.character(start_time), split = ' ')[[1]][1])
#creating a logfile where all output will go
sink(file = paste0(log_folder, date, "_", project_name, "_analysis_log.txt"), split = TRUE)


setwd(path_to_cytomata)
source("./analysis/analysis_functions.R")
setwd(path_to_cytomata)
source("./analysis/analysis_plot_settings.R")
setwd(path_to_cytomata)
source("./analysis/analysis_plots.R")



data_sub_counter <- 0
data_sub <- data_subsets[1] #FOR TESTING, REMOVE LATER
for (data_sub in data_subsets) {
    cat(paste0("\n DATA SUBSET SELECTED IS: ", data_sub, "\n"))
    setwd(subset_folder)
    if (grepl(data_sub, dir())) {
        data_sub_counter <- data_sub_counter + 1

        output_data_sub <- paste0(output_analysis, data_sub, "/")
        ifelse(!dir.exists(output_data_sub), dir.create(output_data_sub), FALSE)
        output_data_sub_analysis <- paste0(output_analysis, data_sub, "/", date, "/")
        ifelse(!dir.exists(output_data_sub_analysis), dir.create(output_data_sub_analysis), FALSE)
        output_clustering <- paste0(output_data_sub_analysis, "clustering", "/")
        ifelse(!dir.exists(output_clustering), dir.create(output_clustering), FALSE)
        output_exploration <- paste0(output_data_sub_analysis, "exploration", "/")
        ifelse(!dir.exists(output_exploration), dir.create(output_exploration), FALSE)
        output_core <- paste0(output_data_sub_analysis, "core", "/")
        ifelse(!dir.exists(output_core), dir.create(output_core), FALSE)



        input <- dir(subset_folder, recursive = TRUE, include.dirs = FALSE)[grepl(data_sub, dir(subset_folder, recursive = TRUE, include.dirs = FALSE))]
        stripped_input <- gsub(paste0(data_sub, "/"), "", input)
        #DUE TO 0 CELLS IN SOME SAMPLES AFTER PRE-GATING
        #REMOVE ALL FILES SMALLER THAN 8 kB (normally 0 or 1 events)
        #you get nasty errors otherwise
        if (length(sapply(strsplit(input[file.size(input) < 8000], "/"), function(x) x[3])) > 0) {
            cat("\n", sapply(strsplit(input[file.size(input) < 8000], "/"), function(x) x[3]), "\n have filesize of less than 8kB, which normally means <5 events and will be removed\n")
            input <- input[file.size(input) > 8000]
        }

        #meta-based filtering out of duplicate anchors here to select input
        duplicated_anchors <- unlist(meta[meta$id %in% anchor_ids & duplicated(meta$id), "fcs"])
        cat("\n", length(duplicated_anchors), "anchor samples detected in meta, they will be omitted\n")
        filtered_meta <- meta[!meta$fcs %in% duplicated_anchors, ]
        
        input_duplicated_anchors <- input[stripped_input %in% duplicated_anchors]
        cat("\n", length(input_duplicated_anchors), "anchor samples detected in folder, they will be omitted\n")
        stripped_input <- stripped_input[!stripped_input %in% duplicated_anchors]
        input <- input[!input %in% input_duplicated_anchors]

        meta_input <- unlist(filtered_meta[filtered_meta$fcs %in% stripped_input, "fcs"])
        if (length(meta_input) < length(filtered_meta$fcs)) {
            warning(paste0("Only ", length(meta_input), " out of ", length(filtered_meta$fcs), " samples in meta after filtering are present in input directory"))
        }
        if (length(meta_input) > length(filtered_meta$fcs)) {
            warning(paste0(length(meta_input), " out of ", length(filtered_meta$fcs), " samples in meta after filtering are present in input directory"))
        }

        final_input <- input[stripped_input %in% meta_input]

        sampling_rate <- sampling_factors[data_sub_counter]

        sampling_rate_changed <- check_sampling_rate_changes()

        #settings for transformation
        asinh_transform <- TRUE
        cofac <- 5
        exprs_set <- inject_fcs(final_input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac,
                                sampling_rate = sampling_rate, silent = FALSE, event_cutoff = event_cutoff)

        new_samples_mode <- 0
        if (sum(filtered_meta$analysis > 1) > 0) {
            new_samples_mode <- 1
            exprs_set$sample_state[exprs_set$sample %in% filtered_meta$fcs[filtered_meta$analysis > 1]] <- "new"
        }

        dropped_samples_mode <- 0
        if (sum(filtered_meta$analysis < 1) > 0) {
            dropped_samples_mode <- 1
            exprs_set$sample_state[exprs_set$sample %in% filtered_meta$fcs[filtered_meta$analysis < 1]] <- "dropped"
        }

        first_run_mode_check()

        #SET FEATURES TO BE USED FOR CLUSTERING AND AUTOMATIC VISUALIZATION ######
        #this is also used for ordering of features in plots
        if (length(dir(meta_folder, pattern = "subset_feature_selection.xlsx")) > 0) {
            subset_feature_selection <- read_xlsx(paste0(meta_folder, "subset_feature_selection.xlsx"))
            clustering_feature_markers <- unlist(strsplit(subset_feature_selection[subset_feature_selection$subset == data_sub, ] %>% pull(features), split = ", ", fixed = TRUE))
            cat("\n Features were set from subset_feature_selection.xlsx table \n")
            cat("\n Features selected for clustering are:\n", clustering_feature_markers, "\n")
        } else if (low_var_feature_removal[1] == 1) {
            ## REMOVE FEATURES WITH LOW VARIANCE ##########################################
            #remove features with low variability from clustering
            #(so that they are not weighted the same as the highly variable ones)
            variances <- apply(exprs_set[, !names(exprs_set) %in% c('sample')],
                                FUN = var, MARGIN=2)
            variances <- variances[order(variances, decreasing = TRUE)]
            cat("\n Features are pre-selected based on variance due to low_var_feature_removal setting \n")
            cat('\n TOP', top_var_features, 'variable markers are:\n')
            print(variances[1:top_var_features])
            cat('\n Features removed due to low variance are:\n', names(variances[-(1:top_var_features)]),'\n')
            remove <- names(variances[-(1:top_var_features)])
            clustering_feature_markers <- setdiff(x = feature_markers, y = remove)
        } else {
            clustering_feature_markers <- feature_markers
        }
        if (first_run_mode > 0) {
            write.csv(clustering_feature_markers, paste0(meta_folder, data_sub, "_first_run_features.csv"), row.names = FALSE)
        }

        feature_input_changed <- check_feature_input_changes()

        
        ## z-normalize the expression levels #########################

        #before clustering and dimension reduction in order to remove the added weight of highly variable features
        #due to variance being normalized this way, all variables are weighted the same in clustering
        #input table with calculated means and standard deviation for each marker (from database_injection.R step)
        mean_stdev <- read.csv(paste0(meta_folder, "meansd.csv"))

        for (i in feature_markers){
            means <- mean_stdev[grepl(paste0("^", i, "$"), mean_stdev[, 1]), "mean"]
            stdev <- mean_stdev[grepl(paste0("^", i, "$"), mean_stdev[, 1]), "stdev"]
            print(paste0("Scaling ", i, " to the mean of ", round(means, 2), " and to the SD of ", round(stdev, 2)))
            exprs_set[, i] <- (exprs_set[, i] - as.numeric(means)) / as.numeric(stdev)
        }


        #plotting samplesizes in absolute values, also export as a csv table
        sample_size_bars()

        exprs_set <- merge_exprs_and_meta()

        batch_size_bars()


        setwd(path_to_cytomata)
        source("./analysis/analysis_clustering.R")
        setwd(path_to_cytomata)
        source("./analysis/analysis_exploration.R")
        setwd(path_to_cytomata)
        source("./analysis/analysis_exploration_addons.R")

        grouping_col_counter <- 0
        for (group in grouping_columns) {
            grouping_col_counter <- grouping_col_counter + 1
            group <- grouping_columns[3] #FOR TESTING, REMOVE LATER =============================================================================================================

            if (grouping_orders[grouping_col_counter] == "NA") {
                group_order <- gtools::mixedsort(unique(exprs_set[, group]))
            } else {
                group_order <- unlist(strsplit(grouping_orders[grouping_col_counter], split = ", ", fixed = TRUE))
            }

            exprs_set[, group] <- factor(exprs_set[, group], levels = group_order, ordered = TRUE)

            if (automatic_palette < 1) {
                group_cols <- unlist(custom_palette[grouping_col_counter])
            } else {
                group_cols <- make_palette_groups(group)
            }




            

            output_group <- paste0(output_core, group, "/")
            dir.create(output_group, showWarnings = FALSE)
            setwd(path_to_cytomata)
            source("./analysis/analysis_core.R")
            setwd(path_to_cytomata)
            source("./analysis/analysis_core_addons.R")            
        }

    } else {
        stop("Folder including given string in the name is not present in fcs/4_subsets!")
    }
}


end_time <- Sys.time()

timediff <- round(as.numeric(gsub('Time difference of ', '', difftime(end_time, start_time, units = "hours"))), 2)

cat(paste0(date, '_', naming, ' analysis run ended successfully\n', 'Total time elapsed (in hours): ', timediff),'\n')

sink()