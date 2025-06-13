# IMPORTANT
# ON WSL, INCREASE THE MEMORY LIMIT AND SWAP FILE AS MUCH AS POSSIBLE
# CAUSE IT WILL CRASH WITH ANY MEANINGFUL AMOUNT OF DATA
# https://learn.microsoft.com/en-us/windows/wsl/wsl-config


library(arrow)
library(dplyr)

setwd(path_to_cytomata)
source("./analysis/analysis_functions.R")
source("./analysis/analysis_plot_settings.R")
source("./analysis/analysis_plots.R")

start_time <- Sys.time()
date <- gsub('-', '', strsplit(x = as.character(start_time), split = ' ')[[1]][1])

setwd(norm_folder)
ds <- arrow::open_dataset("parquet")

backbone_markers <- c("CD45", "CD3", "CD8", "CD19", "CD14", "CD16", "CD56", "CD11c", "HLADR", "CD11b", "CD64", "CD1c", "CD123", "CD15", "CD33")

# see what backbone markers are present in the dataset
backbone_markers <- backbone_markers[backbone_markers %in% colnames(ds)]

clustering_feature_markers <- backbone_markers

exprs_set <- ds %>%
    dplyr::select(all_of(backbone_markers)) %>%
    collect() %>%
    as.data.frame()

sampling_rate <- 1
clustering_mode <- "do_clustering"
clustering_engine <- "fs"
clustering_k <- 20
fs_n_dims <- 10
ccp_delta_cutoff <- 0.02
umap_n <- 15
umap_min_dist <- 0.5

data_sub <- "total_data"
output_data_sub <- paste0(output_analysis, data_sub, "/")
dir.create(output_data_sub, showWarnings = FALSE, recursive = TRUE)

cluster_ids <- do_clustering()
cluster_ids$cell_id <- rownames(cluster_ids)
exprs_set$cell_id <- rownames(exprs_set)
exprs_set <- merge_exprs_and_clusters()

rm(cluster_ids)
gc()
#produces plots for cluster characterization and diagnostics
output_clustering <- paste0(output_data_sub, "clustering/")
dir.create(output_clustering, showWarnings = FALSE, recursive = TRUE)
do_clustering_diagnostics()

#gives the possibility to annotate, merge and delete clusters
exprs_set <- merge_and_annotate()


feature_input_changed <- FALSE
sampling_rate_changed <- FALSE

umap_coords <- do_umap()

exprs_set <- merge_exprs_and_umap()

module <- "total_data"
setwd(output_data_sub)

is_meta_annot_available <- sum(grepl("meta_cluster_annotation", colnames(exprs_set))) > 0

umap_plot(grouping_var = "meta_cluster_id", module = module, labels = TRUE)
if (is_meta_annot_available) {
    umap_plot(grouping_var = "meta_cluster_annotation", module = module, labels = TRUE)
}

umap_facet(grouping_var = "meta_cluster_id", module = module, column_number = 4, equal_sampling = FALSE)
if (is_meta_annot_available) {
    umap_facet(grouping_var = "meta_cluster_annotation", module = module, column_number = 4, equal_sampling = FALSE)
}
umap_expressions(grouping_var = NULL, module = module, column_number = 4)




# YOU HAVE TO SET THIS MANUALLY IF YOU WANT TO DO THIS
# writes subsetted data to .fcs files
# untested, cause UMAP calculation was SUUUUUPER SLOW, but it should work
subset_annot_pops <- FALSE
if (subset_annot_pops && is_meta_annot_available) {

    # add cell_id column to ds
    ds <- ds %>%
        dplyr::mutate(cell_id = row_number())


    data_subs <- unique(exprs_set$meta_cluster_annotation)

    for (data_sub in data_subs) {
        data_sub_dir <- paste0(output_data_sub, data_sub, "/")
        dir.create(data_sub_dir, showWarnings = FALSE, recursive = TRUE)

        temp_set <- exprs_set[exprs_set$meta_cluster_annotation == data_sub, ]


        for (sample in unique(temp_set$sample_id)) {
            temp_sample_sub_index <- temp_set[temp_set$sample_id == sample, "cell_id"]

            data_to_write <- ds[ds$cell_id %in% temp_sample_sub_index, ] %>%
                collect() %>%
                as.data.frame()

            if (nrow(data_to_write) > 0) {
                fcs_file <- paste0(data_sub_dir, sample)
                flowCore::write.FCS(flowCore::flowFrame(data_to_write), fcs_file)
            }

        }
    }
}