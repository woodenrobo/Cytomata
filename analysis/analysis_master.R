#APPLYING SETTINGS ######
grouping_columns <- unlist(strsplit(settings$value[settings$setting == "grouping_columns"], split = ", ", fixed = TRUE))
data_subsets <- unlist(strsplit(settings$value[settings$setting == "data_subsets"], split = ", ", fixed = TRUE))
sampling_factors <- as.numeric(unlist(strsplit(settings$value[settings$setting == "sampling"], split = ", ", fixed = TRUE)))
low_var_feature_removal <- as.numeric(unlist(strsplit(settings$value[settings$setting == "low_var_feature_removal"], split = ", ", fixed = TRUE)))
if (length(low_var_feature_removal) > 1 & low_var_feature_removal[1] == 1) {
    top_var_features <- low_var_feature_removal[2]
}
if (length(low_var_feature_removal) == 1 & low_var_feature_removal[1] == 1) {
    top_var_features <- 20
}
clustering_engine <- settings$value[settings$setting == "clustering_engine"]
clustering_k <- as.numeric(unlist(strsplit(settings$value[settings$setting == "clustering_k"], split = ", ", fixed = TRUE)))
umap_n <- as.numeric(unlist(strsplit(settings$value[settings$setting == "umap_n"], split = ", ", fixed = TRUE)))
umap_min_dist <- as.numeric(unlist(strsplit(settings$value[settings$setting == "umap_min_dist"], split = ", ", fixed = TRUE)))


for (data_sub in data_subsets) {
    cat(paste0("\n DATA SUBSET SELECTED IS: ", data_sub, "\n"))
    setwd(subset_folder)
    if (grepl(data_sub, dir())) {
        input <- dir(recursive = TRUE, include.dirs = FALSE)[grepl(data_sub, dir(recursive = TRUE, include.dirs = FALSE))]
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

        sampling_rate <- 1
        #settings for transformation
        asinh_transform <- TRUE
        cofac <- 5
        exprs_set <- inject_fcs(meta_input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)

    } else {
        stop("Folder including given string in the name is not present in fcs/4_subsets!")
    }
}


