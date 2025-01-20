merge_exprs_and_meta <- function() {
    temp <- left_join(x = exprs_set, y = meta, by = join_by(sample == fcs))
    colnames(temp)[colnames(temp) %in% "sample"] <- "fcs"
    return(temp)
}

first_run_mode_check <- function() { 
    if (interactive() && first_run_mode > 0 && new_samples_mode > 0 || interactive() && first_run_mode > 0 && dropped_samples_mode > 0) {
        answer <- readline(paste0("Did you forget to switch off first_run_mode? New or dropped samples detected in meta\n",
                        "Change settings table in\n",
                        "<Cytomata_folder>/\n",
                        "and type \"continue\" when you are ready\n"))
    } else if (first_run_mode > 0 && new_samples_mode > 0 || first_run_mode > 0 && dropped_samples_mode > 0) {
        cat("New or dropped samples detected in meta. You forgot to switch off first_run_mode! We will switch it off for you.\n")
        answer <- "continue"
    } else if (first_run_mode > 0 && new_samples_mode == 0 && dropped_samples_mode == 0) {
        answer <- "skip"
    } else {
        answer <- "continue"
    }

    if (answer == "continue") {
        cat("Analysis continues with new or dropped samples\n")
        first_run_mode <- 0
    } else if (answer != "skip") {
        cat("\n\nIt seems you have typed an incorrect answer!\n\n")
        first_run_mode_check()
    }
    answer <- NULL
}

set_clustering_mode <- function() {
    #check for ad-hoc (after first clustering) additions or removal of samples
    #column "analysis" in metafile has to contain "2" for samples that were added after the first round of clustering
    #column "analysis" in metafile has to contain "0" for samples that were removed after the first round of clustering

    #restore previous ad-hoc clustering if already done
    if (sum(grepl(paste0("clustering_ad_hoc_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == FALSE) {
        clustering_mode <- "restore_ad_hoc"
    #restore previous clustering if already done
    } else if (sum(grepl(paste0("clustering_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == FALSE) {
        clustering_mode <- "restore_clustering"
    #repeat previous ad-hoc clustering if even more samples are added
    } else if (sum(grepl(paste0("clustering_ad_hoc_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == TRUE) {
        clustering_mode <- "repeat_ad_hoc"
    #do ad-hoc (after first clustering) clustering if more samples are added and clusters need to be preserved
    #column "analysis" in metafile has to contain "2" for samples that were added after the first round of clustering
    #if clustering does not need to be preserved, remove old clustering results from "Cytomata_data/<project_name>/output/analysis/<data_subset>/" folder
    #(or change the sampling rate and see how it feels when a script starts screaming at you)
    } else if (sum(grepl(paste0("clustering_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == TRUE) {
        clustering_mode <- "do_ad_hoc"
    } else if (sum(grepl(paste0("clustering_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) == 0) {
        clustering_mode <- "do_clustering"
    }

    if (sum(grepl(paste0("clustering_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) == 0 && sum(grepl(paste0("clustering"), dir(output_data_sub)) == TRUE) > 0) {
        cat("Warning: Clustering engine chosen is different from the one, which already has results present in <data_subset> folder\n")
        cat("Clustering engine", clustering_engine, "will be used\n")
    }

    if (feature_input_changed > 0) {
        cat("Warning: Features selected have changed!\n")
        if (feature_input_changed == 1) {
            cat("Clustering will be repeated de novo!\n")
            if (sum(grepl(paste0("clustering_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == TRUE) {
                clustering_mode <- "do_ad_hoc"
            } else if (sum(grepl(paste0("clustering_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) == 0) {
                clustering_mode <- "do_clustering"
            }
        }
        if (feature_input_changed == 2) {
            cat("Clustering will be restored but plots will use new features!\n")
            if (sum(grepl(paste0("clustering_ad_hoc_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == FALSE) {
                clustering_mode <- "restore_ad_hoc"
            #restore previous clustering if already done
            } else if (sum(grepl(paste0("clustering_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == FALSE) {
                clustering_mode <- "restore_clustering"
            #repeat previous ad-hoc clustering if even more samples are added
            } else if (sum(grepl(paste0("clustering_ad_hoc_", clustering_engine, ".csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == TRUE) {
                clustering_mode <- "repeat_ad_hoc"
            #do ad-hoc (after first clustering) clustering if more samples are added and clusters need to be preserved
            #column "analysis" in metafile has to contain "2" for samples that were added after the first round of clustering
            #if clustering does not need to be preserved, remove old clustering results from "Cytomata_data/<project_name>/output/analysis/<data_subset>/" folder
            #(or change the sampling rate and see how it feels when a script starts screaming at you)
            }
        }
    }

    if (sampling_rate_changed > 0) {
        cat("Warning: Sampling rate was changed, clustering will be repeated!\n")
        clustering_mode <- "do_clustering"
    }
    return(clustering_mode)
}


drop_resampled_events <- function() {
    if (sampling_rate > 1) {
        output <- exprs_set[!which(exprs_set$resampled == "yes"), ]
        cat("Resampled events were removed\n")
    } else {
        output <- exprs_set
    }
    return(output)
}


merge_exprs_and_clusters <- function() {
    if ("meta_cluster_id" %in% colnames(exprs_set) == FALSE) {
        temp <- left_join(exprs_set, cluster_ids, by = "cell_id")
    } else {
        temp <- exprs_set
    }
    return(temp)
}


do_clustering <- function() {
    if (clustering_mode == "restore_ad_hoc") {
        cat("Using", clustering_engine, "clustering engine\n")
        cat("\nClustering results restored from ad-hoc clustering\n")
        cluster_ids <- read.csv(dir(output_data_sub, full.names = TRUE)[grepl(paste0("clustering_ad_hoc_", clustering_engine, ".csv"), dir(output_data_sub))]) %>% subset(select = -X)
    }
    if (clustering_mode == "restore_clustering") {
        cat("Using", clustering_engine, "clustering engine\n")
        cat("\nClustering results restored\n")
        cluster_ids <- read.csv(dir(output_data_sub, full.names = TRUE)[grepl(paste0("clustering_", clustering_engine, ".csv"), dir(output_data_sub))]) %>% subset(select = -X)
    }
    if (clustering_mode == "repeat_ad_hoc") {
        cat("Using", clustering_engine, "clustering engine\n")
        cat("\nNew samples detected, ad-hoc clustering procedure will be repeated\n")

        if (clustering_engine == "fs") {
            som <- readRDS(file = paste0(output_data_sub, "fs_model.rds"))
            som <- FlowSOM::NewData(som, exprs_set[exprs_set$analysis == 2, colnames(exprs_set) %in% clustering_feature_markers])
        }
        if (clustering_engine == "fs_manual") {
            som <- readRDS(file = paste0(output_data_sub, "fs_model.rds"))
            som <- FlowSOM::NewData(som, exprs_set[exprs_set$analysis == 2, colnames(exprs_set) %in% clustering_feature_markers])
        }
        if (clustering_engine == "pg") {
            #use THIS to implement properly
            #https://shihchingfu.github.io/knn-caret-example/

            train_data <- exprs_set[!grepl(paste(new_samples, collapse='|'), exprs_set$sample),]
            true_classes <- cluster_assignment
            test_data <- exprs_set[grepl(paste(new_samples, collapse='|'), exprs_set$sample),]
            train_data <- train_data[, !names(train_data) %in% c('sample')]
            test_data <- test_data[, !names(test_data) %in% c('sample')]
            new_classes <- class::knn(train_data, test_data, true_classes)
            #knn_model <- class::knn(train_data, test_data, true_classes)
            cluster_assignment <- c(true_classes, new_classes)
            #tab <- table(new_classes,true_classes)
        }


    }
    if (clustering_mode == "do_ad_hoc") {
        cat("Using", clustering_engine, "clustering engine\n")
        cat("\nNew samples detected, starting ad-hoc clustering procedure\n")

        if (clustering_engine == "fs") {
            som <- readRDS(file = paste0(output_data_sub, "fs_model.rds"))
            som <- FlowSOM::NewData(som, exprs_set[exprs_set$analysis == 2, colnames(exprs_set) %in% clustering_feature_markers])
            optimal_k <- as.numeric(read.csv(file = paste0(output_data_sub, "first_run_fs_optimal_k.csv"))[-1])
            metaclusters <- ConsensusClusterPlus(t(som$map$codes), maxK = clustering_k, reps = 1000, distance = "euclidean", plot = "pdf", title = paste0(clustering_engine, "_ad_hoc_mode_consensus"))
            #WIP *************************************************************************************************************
        }
        if (clustering_engine == "fs_manual") {
            som <- readRDS(file = paste0(output_data_sub, "fs_model.rds"))
            som <- FlowSOM::NewData(som, exprs_set[exprs_set$analysis == 2, colnames(exprs_set) %in% clustering_feature_markers])
            #WIP *************************************************************************************************************
        }
        if (clustering_engine == "pg") {
            #use THIS to implement properly
            #https://shihchingfu.github.io/knn-caret-example/

            #WIP *************************************************************************************************************

            train_data <- exprs_set[!grepl(paste(new_samples, collapse='|'), exprs_set$sample),]
            true_classes <- cluster_assignment
            test_data <- exprs_set[grepl(paste(new_samples, collapse='|'), exprs_set$sample),]
            train_data <- train_data[,!names(train_data) %in% c('sample')]
            test_data <- test_data[,!names(test_data) %in% c('sample')]
            new_classes <- class::knn(train_data, test_data, true_classes)
            #knn_model <- class::knn(train_data, test_data, true_classes)
            cluster_assignment <- c(true_classes, new_classes)
            #tab <- table(new_classes,true_classes)
        }


    }
    if (clustering_mode == "do_clustering") {
        cat("Using", clustering_engine, "clustering engine\n")
        cat("\nStarting clustering\n")

        if (clustering_engine == "fs") {
            cat("\nClustering with FlowSOM\n")

            xdim <- fs_n_dims
            ydim <- fs_n_dims
            fsom <- FlowSOM::ReadInput(flowFrame(as.matrix(exprs_set[, colnames(exprs_set) %in% clustering_feature_markers])), scale = FALSE, scaled.center = FALSE, scaled.scale = FALSE)
            som <- FlowSOM::BuildSOM(fsom, xdim = xdim, ydim = ydim)
            cat("SOM model saved for potential ad-hoc data addition\n")
            saveRDS(som, file = paste0(output_data_sub, "fs_model.rds"))
            cat("Metacustering using ConsensusClusterPlus\n")
            setwd(output_data_sub)
            metaclusters <- ConsensusClusterPlus(t(som$map$codes), maxK = clustering_k, reps = 1000, distance = "euclidean", plot = "pdf", title = paste0(clustering_engine, "_mode_consensus"))

            triangle <- function(m) {
                n <- dim(m)[1]
                nm <- matrix(0, ncol = n, nrow = n)
                fm <- m
                nm[upper.tri(nm)] <- m[upper.tri(m)] #only upper half
                fm <- t(nm) + nm
                diag(fm) <- diag(m)
                nm <- fm
                nm[upper.tri(nm)] <- NA
                diag(nm) <- NA
                vm <- m[lower.tri(nm)]
                return(vm)
            }

            areaK <- c()
            for (i in 2:clustering_k){
                v <- triangle(metaclusters[[i]][["ml"]])

                #empirical CDF distribution. default number of breaks is 100    
                h <- hist(v, plot = FALSE, breaks = seq(0, 1, by = 1 / 100))
                h$counts <- cumsum(h$counts) / sum(h$counts)

                #calculate area under CDF curve, by histogram method.
                thisArea <- 0
                for (bi in 1:(length(h$breaks)-1)){
                    thisArea <- thisArea + h$counts[bi] * (h$breaks[bi + 1] - h$breaks[bi]) #increment by height by width
                    bi <- bi + 1
                }
                areaK <- c(areaK, thisArea)
            }
            
            #area under CDF change.
            deltaK <- areaK[1] #initial auc at k=2
            for (i in 2:(length(areaK))) {
                #proportional increase relative to prior K.
                deltaK <- c(deltaK, (areaK[i] - areaK[i - 1]) / areaK[i - 1])
            }

            optimal_k <- min(which(deltaK < ccp_delta_cutoff)) - 1

            # get cluster codes
            k <- xdim * ydim
            mcs <- seq_len(clustering_k)[-1]

            # construct data.frame of clustering codes
            codes <- data.frame(seq_len(k), purrr::map(metaclusters[-1], "consensusClass"))
            codes <- mutate_all(codes, function(u) factor(u, levels = sort(unique(u))))
            colnames(codes) <- c(sprintf("som%s", k), sprintf("meta%s", mcs))

            #storing SOM and metacluster assignments using optimal k
            cluster_ids <- c()
            cluster_ids$som_cluster_id <- factor(som$map$mapping[, 1])
            cluster_ids$meta_cluster_id <- NA
            for (som_clust in seq(k)){
                cluster_ids$meta_cluster_id[cluster_ids$som_cluster_id == som_clust] <- codes[codes$som100 == som_clust, paste0("meta", optimal_k)]
            }
            cluster_ids$cell_id <- exprs_set$cell_id
            cluster_ids <- as.data.frame(cluster_ids)


            cat("Optimal k of", optimal_k, "detected, please refer to", clustering_engine, "mode consensus folder for diagnostics\n")
            cat("Cluster assignment saved for faster post-processing (adjusting figures etc.) runs\n")
            write.csv(cluster_ids, file = paste0(output_data_sub, "clustering_", clustering_engine, ".csv"))
            write.csv(sampling_rate, file = paste0(output_data_sub, "first_run_sampling_rate.csv"))
            write.csv(optimal_k, file = paste0(output_data_sub, "first_run_fs_optimal_k.csv"))
        }
        if (clustering_engine == "fs_manual") {
            cat("\nClustering with FlowSOM\n")
            xdim <- fs_n_dims
            ydim <- fs_n_dims
            fsom <- FlowSOM::ReadInput(flowFrame(as.matrix(exprs_set[, colnames(exprs_set) %in% clustering_feature_markers])), scale = FALSE, scaled.center = FALSE, scaled.scale = FALSE)
            som <- FlowSOM::BuildSOM(fsom, xdim = xdim, ydim = ydim)
            cat("SOM model saved for potential ad-hoc data addition\n")
            saveRDS(som, file = paste0(output_data_sub, "fs_model.rds"))
            cat("Metacustering using ConsensusClusterPlus\n")
            setwd(output_data_sub)
            metaclusters <- ConsensusClusterPlus(t(som$map$codes), maxK = clustering_k, reps = 100, distance = "euclidean", plot = "pdf", title = paste0(clustering_engine, "mode consensus"))

            # get cluster codes
            k <- xdim * ydim
            mcs <- seq_len(clustering_k)[-1]

            # construct data.frame of clustering codes
            codes <- data.frame(seq_len(k), purrr::map(metaclusters[-1], "consensusClass"))
            codes <- mutate_all(codes, function(u) factor(u, levels = sort(unique(u))))
            colnames(codes) <- c(sprintf("som%s", k), sprintf("meta%s", mcs))

            #storing SOM and metacluster assignments using optimal k
            cluster_ids <- c()
            cluster_ids$som_cluster_id <- factor(som$map$mapping[, 1])
            cluster_ids$meta_cluster_id <- NA
            for (som_clust in seq(k)){
                cluster_ids$meta_cluster_id[cluster_ids$som_cluster_id == som_clust] <- codes[codes$som100 == som_clust, paste0("meta", optimal_k)]
            }
            cluster_ids$cell_id <- exprs_set$cell_id
            cluster_ids <- as.data.frame(cluster_ids)

            cat("K of", clustering_k, "was selected in settings, please refer to", clustering_engine, "mode consensus folder for diagnostics\n")
            cat("Cluster assignment saved for faster post-processing (adjusting figures etc.) runs\n")
            write.csv(cluster_ids, file = paste0(output_data_sub, "clustering_", clustering_engine, ".csv"))
            write.csv(sampling_rate, file = paste0(output_data_sub, "first_run_sampling_rate.csv"))
        }
        if (clustering_engine == "pg") {
            cat("\nClustering with PhenoGraph\n")
            cat("K nearest neighbors of", clustering_k, "was selected in settings\n")
            cluster_assignment <- cytof_cluster(xdata = exprs_set[, colnames(exprs_set) %in% clustering_feature_markers], method = "Rphenograph", Rphenograph_k = clustering_k)
            cat(length(unique(cluster_assignment)), "clusters were detected\n")

            cluster_ids <- c()
            cluster_ids$meta_cluster_id <- cluster_assignment
            cluster_ids$cell_id <- exprs_set$cell_id
            cluster_ids <- as.data.frame(cluster_ids)


            cat("Cluster assignment saved for faster post-processing (adjusting figures etc.) runs\n")
            write.csv(cluster_ids, file = paste0(output_data_sub, "clustering_", clustering_engine, ".csv"))
            write.csv(sampling_rate, file = paste0(output_data_sub, "first_run_sampling_rate.csv"))
        }
    }
    return(cluster_ids)
}


do_clustering_diagnostics <- function() {

    cluster_size_bars()
    cluster_prop_bars()

    cluster_expr_heatmap(expression_setting = "means", scale = TRUE)
    cluster_expr_heatmap(expression_setting = "means", scale = FALSE)
    cluster_expr_heatmap(expression_setting = "medians", scale = TRUE)
    cluster_expr_heatmap(expression_setting = "medians", scale = FALSE)

    cluster_expr_densities()

}

do_clustering_diagnostics_no_dropped <- function() {
    
    cluster_size_bars(after_dropping = TRUE)
    cluster_prop_bars(after_dropping = TRUE)

    cluster_expr_heatmap(expression_setting = "means", scale = TRUE, after_dropping = TRUE)
    cluster_expr_heatmap(expression_setting = "means", scale = FALSE, after_dropping = TRUE)
    cluster_expr_heatmap(expression_setting = "medians", scale = TRUE, after_dropping = TRUE)
    cluster_expr_heatmap(expression_setting = "medians", scale = FALSE, after_dropping = TRUE)

    cluster_expr_densities(after_dropping = TRUE)

}

continue_or_recluster <- function() {
    options(warn = 1)
    if (interactive() && first_run_mode > 0) {
        answer <- readline(paste0("Are you satisfied with clustering results?\n",
                        "If yes, type \"continue\"\n",
                        "If not, change clustering settings table in\n",
                        "<Cytomata_folder>/\n",
                        "and type \"recluster\" when you are ready\n"))
    } else {
        answer <- "continue"
    }

    if (answer == "recluster") {
        clustering_mode <- "do_clustering"
        clustering_engine <- settings$value[settings$setting == "clustering_engine"]
        clustering_k <- as.numeric(unlist(strsplit(settings$value[settings$setting == "clustering_k"], split = ", ", fixed = TRUE)))
        fs_n_dims <- as.numeric(settings$value[settings$setting == "fs_n_dims"])
        ccp_delta_cutoff <- as.numeric(settings$value[settings$setting == "ccp_delta_cutoff"])
        do_clustering()
        do_clustering_diagnostics()
        continue_or_recluster()
    } else if (answer == "continue") {
        cat("Continuing with current clustering results\n")
    } else {
        cat("\n\nIt seems you have typed an incorrect answer!\n\n")
        continue_or_recluster()
    }
    answer <- NULL
}


merge_and_annotate <- function() {
    if (first_run_mode > 0 && sum(dir(output_data_sub)[grepl("cluster_merging_and_annotation.xlsx", dir(output_data_sub))] == 0)) {
        cat("Creating cluster merging and annotation file\n")
        cluster_annot <- as.data.frame(seq_along(unique(exprs_set$meta_cluster_id)))
        colnames(cluster_annot) <- "original_clusters"
        cluster_annot$merge_with <- "NA"
        cluster_annot$annotation <- "NA"
        cluster_annot$guide <- "NA"
        cluster_annot$guide[1] <- "merge_with: put number of cluster that you want to merge this cluster into (merging is done from highest number to lowest)" 
        cluster_annot$guide[2] <- "merge_with: put 0 in order to delete the cluster"
        cluster_annot$guide[3] <- "annotation: here you can annotate your clusters"   
        cluster_annot$guide[4] <- "annotation: If you put any values other than \"NA\" into this column, plots with annotation will be created"
        xlsx::write.xlsx(cluster_annot, file = paste0(output_data_sub, "cluster_merging_and_annotation.xlsx"))
    }

    dropped_events <<- c()
    exprs_set <- skip_or_merge_and_annotate()
    
    return(exprs_set)
}


skip_or_merge_and_annotate <- function() {

    if (interactive() && first_run_mode > 0) {
        answer <- readline(paste0("Do you want to manually merge, delete or annotate clusters?\n",
                        "If yes, go to <project_folder>/output/analysis/<data_subset>/cluster_merging_and_annotation.xlsx\n",
                        "Type \"continue\" when you are ready\n",
                        "Or type \"skip\" to skip this step for now\n"))
    } else {
        answer <- "skip"
    }

    if (answer == "continue") {
        cluster_annot <- readxl::read_excel(paste0(output_data_sub, "cluster_merging_and_annotation.xlsx"))[-1]
        cat("Cluster annotations, merging and deletion parameters have been applied, if set.\n")
        exprs_set <- apply_annotation(cluster_annot)
        exprs_set <- merge_or_delete_clusters(exprs_set, cluster_annot)
    } else if (answer == "skip") {
        cat("Continuing without merging, deleting or annotating\n")
    } else {
        cat("\n\nIt seems you have typed an incorrect answer!\n\n")
        skip_or_merge_and_annotate()
    }

    if (first_run_mode < 1) {
        cluster_annot <- readxl::read_excel(paste0(output_data_sub, "cluster_merging_and_annotation.xlsx"))[-1]
        cat("Cluster annotations, merging and deletion parameters have been applied, if set.\n")
        exprs_set <- apply_annotation(cluster_annot)
        exprs_set <- merge_or_delete_clusters(exprs_set, cluster_annot)
    }
    answer <- NULL

    return(exprs_set)
}


apply_annotation <- function(cluster_annot) {
    temp <- rep("NA", nrow(exprs_set))
    for (i in cluster_annot$original_clusters) {
        temp[exprs_set$meta_cluster_id == i] <- cluster_annot[cluster_annot$original_clusters == i, "annotation"]
    }
    temp <- as.character(temp)
    exprs_set$meta_cluster_annotation <- temp

    return(exprs_set)
}


merge_or_delete_clusters <- function(exprs_set, cluster_annot) {
    rev_cluster_annot <- cluster_annot[sort(cluster_annot$original_clusters, decreasing = TRUE), ]
    rev_cluster_annot$merge_with <- as.numeric(rev_cluster_annot$merge_with)
    temp <- exprs_set$meta_cluster_id
    for (i in rev_cluster_annot$original_clusters) {
        from <- i
        to <- unlist(rev_cluster_annot[rev_cluster_annot$original_clusters == i, "merge_with"])
        if (is.na(to) == FALSE && to != 0) {
            temp[temp == to | temp == from] <- to
            temp[temp > from] <- temp[temp > from] - 1
        } else if (is.na(to) == FALSE && to == 0) {
            temp[temp == from] <- to
            temp[temp > from] <- temp[temp > from] - 1           
        }
    }
    if (sum(grepl("^0$", temp)) > 0) {
        dropped_events <<- exprs_set$cell_id[grepl("^0$", temp)]
    }

    exprs_set$meta_cluster_id <- temp
    return(exprs_set)
}


do_pca <- function() {
    pca <- prcomp(exprs_set[, colnames(exprs_set) %in% clustering_feature_markers], scale. = TRUE, center = TRUE)
    return(pca)
}


merge_exprs_and_pca <- function() {
    pca_coords <- as.data.frame(pca$x)
    pca_coords$cell_id <- exprs_set$cell_id
    if ("PC1" %in% colnames(exprs_set) == FALSE) {
        temp <- left_join(exprs_set, pca_coords[, c("PC1", "PC2", "PC3", "PC4", "cell_id")], by = "cell_id")
    } else {
        temp <- exprs_set
    }

    return(temp)
}


do_pca_plots <- function(module) {
    pca_biplot(grouping_var = "batch", dims = c(1, 2), module = module)
    pca_biplot(grouping_var = "batch", dims = c(3, 4), module = module)
    pca_biplot(grouping_var = "id", dims = c(1, 2), module = module)
    pca_biplot(grouping_var = "id", dims = c(3, 4), module = module)
    pca_biplot(grouping_var = "meta_cluster_id", dims = c(1, 2), module = module)
    pca_biplot(grouping_var = "meta_cluster_id", dims = c(3, 4), module = module)
    
}


do_umap <- function() {
    if (sum(grepl(paste0("umap_coords.csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == FALSE) {
        umap_mode <- "restore_umap"
    } else {
        umap_mode <- "do_umap"
    }

    if (feature_input_changed > 0) {
        cat("Warning: Features selected have changed!\n")
        if (feature_input_changed == 1) {
            cat("UMAP will be repeated de novo!\n")
            umap_mode <- "do_umap"
        }
        if (feature_input_changed == 2) {
            if (sum(grepl(paste0("umap_coords.csv"), dir(output_data_sub)) == TRUE) > 0 && new_samples_mode == FALSE) {
                cat("UMAP will be restored but plots will use new features!\n")
                umap_mode <- "restore_umap"
            } else {
                umap_mode <- "do_umap"
            }
        }
    }

    if (sampling_rate_changed > 0) {
        cat("Warning: Sampling rate was changed, UMAP will be recalculated!\n")
        umap_mode <- "do_umap"
    }



    #restore UMAP from .csv if available
    if (umap_mode == "do_umap"){
        cat('Calculating UMAP\n')
        set.seed(1234)
        umap_coords <- data.frame(uwot::umap(exprs_set[, colnames(exprs_set) %in% clustering_feature_markers], n_neighbors = umap_n, min_dist = umap_min_dist, metric = 'euclidean'))
        colnames(umap_coords) <- c('UMAP1','UMAP2')
        cat('UMAP done\n')
        umap_coords$cell_id <- exprs_set$cell_id
        write.csv(umap_coords, paste0(output_data_sub, "umap_coords.csv"))
    } else if (umap_mode == "restore_umap") {
        cat('\nUMAP restored from .csv\n')
        umap_coords <- read.csv(paste0(output_data_sub, "umap_coords.csv"))[, -1]
    }

    return(umap_coords)
}

merge_exprs_and_umap <- function() {
    if ("UMAP1" %in% colnames(exprs_set) == FALSE) {
        temp <- left_join(exprs_set, umap_coords, by = "cell_id")
    } else {
        temp <- exprs_set
    }

    return(temp)
}


remove_dropped_events <- function() {
    if (length(dropped_events) > 0) {
        temp <- exprs_set[!exprs_set$cell_id %in% dropped_events, ]
        cat(paste0(length(dropped_events), " events from deleted clusters removed\n"))
    }

    if (dropped_samples_mode > 0) {
        temp <- temp[!temp$sample_state == "dropped", ]
        cat(paste0(sum(exprs_set$sample_state == "dropped"), " events from deleted samples removed\n"))
    }

    if (length(dropped_events) > 0 || dropped_samples_mode > 0) {
        return(temp)
    } else {
        return(exprs_set)
    }
}


do_umap_plots <- function(module) {
    if (module == "exploration") {
        umap_plot(grouping_var = "batch", module = module, labels = TRUE)
        umap_plot(grouping_var = "id", module = module, labels = FALSE)
        umap_plot(grouping_var = "meta_cluster_id", module = module, labels = TRUE)
        if (sum(grepl("meta_cluster_annotation", colnames(exprs_set))) > 0) {
            umap_plot(grouping_var = "meta_cluster_annotation", module = module, labels = TRUE)
        }

        umap_facet(grouping_var = "batch", module = module, column_number = 4, equal_sampling = FALSE)
        umap_facet(grouping_var = "meta_cluster_id", module = module, column_number = 4, equal_sampling = FALSE)
        if (sum(grepl("meta_cluster_annotation", colnames(exprs_set))) > 0) {
            umap_facet(grouping_var = "meta_cluster_annotation", module = module, column_number = 4, equal_sampling = FALSE)
        }

        umap_expressions(grouping_var = NULL, module = module, column_number = 4)
        umap_expressions(grouping_var = "batch", module = module, column_number = 4)

        if (new_samples_mode > 0) {
            umap_plot(grouping_var = "analysis", module = module, labels = TRUE)
            umap_facet(grouping_var = "analysis", module = module, equal_sampling = FALSE)
        }

    } else if (module == "core") {
        umap_facet(grouping_var = group, module = module, equal_sampling = TRUE)
        umap_expressions(grouping_var = group, module = module, column_number = 4)
    }

}


continue_or_recalculate_umap <- function() {
    # if (interactive() && first_run_mode > 0) {
    #     answer <- readline(paste0("Are you satisfied with clustering results?\n",
    #                     "If yes, type \"continue\"\n",
    #                     "If not, change clustering settings table in\n",
    #                     "<Cytomata_folder>/\n",
    #                     "and type \"recluster\" when you are ready\n"))
    # } else {
    #     answer <- "continue"
    # }

    # if (answer == "recluster") {
    #     clustering_mode <- "do_clustering"
    #     update_settings()
    #     clustering_engine <- settings$value[settings$setting == "clustering_engine"]
    #     clustering_k <- as.numeric(unlist(strsplit(settings$value[settings$setting == "clustering_k"], split = ", ", fixed = TRUE)))
    #     fs_n_dims <- as.numeric(settings$value[settings$setting == "fs_n_dims"])
    #     ccp_delta_cutoff <- as.numeric(settings$value[settings$setting == "ccp_delta_cutoff"])
    #     do_clustering()
    #     do_clustering_diagnostics()
    #     continue_or_recluster()
    # } else if (answer == "continue") {
    #     cat("Continuing with current clustering results\n")
    # } else {
    #     cat("\n\nIt seems you have typed an incorrect answer!\n\n")
    #     continue_or_recluster()
    # }
    # answer <- NULL
}


summary_table <- function(data = exprs_set, grouping_var, selected_features = NULL, stat = "mean") {
  
  if (stat == "mean") {
    temp <- data %>% 
      group_by(across(all_of(grouping_var))) %>% 
      summarise(across(all_of(selected_features), mean, na.rm = TRUE), .groups = 'drop')
      return(temp)
  } else if (stat == "median") {
    temp <- data %>% 
      group_by(across(all_of(grouping_var))) %>% 
      summarise(across(all_of(selected_features), median, na.rm = TRUE), .groups = 'drop')
      return(temp)
  } else if (stat == "count") {
    temp <- data %>% 
      group_by(across(all_of(grouping_var))) %>% 
      summarise(count = n(), .groups = 'drop')
      return(temp)
  } else if (stat == "n_size") {
    temp <- data %>% 
      group_by(across(all_of(grouping_var))) %>% 
      summarise(n_size = n_distinct(id), .groups = 'drop')
      return(temp)
  } else {
    cat("Incorrect options set\n")
  }
}


calculate_cluster_proportions <- function(cluster_var = "meta_cluster_id", selected_clusters = NULL) {
    all_clusters <- unique(exprs_set[[cluster_var]])
    if (is.null(selected_clusters)) {
            cluster_proportions <- summary_table(exprs_set, c(group, "id", cluster_var), selected_features = NULL, "count") %>%
                                    tidyr::complete(id, !!sym(cluster_var) := all_clusters, fill = list(count = 0)) %>%
                                    tidyr::fill(!!sym(group), .direction = "downup") %>%
                                    group_by(id) %>%
                                    mutate(prop = count / sum(count) * 100) %>%
                                    ungroup()
    } else {
            all_clusters <- intersect(all_clusters, selected_clusters)
            cluster_proportions <- summary_table(exprs_set, c(group, "id", cluster_var), selected_features = NULL, "count") %>%
                                    dplyr::filter(!!sym(cluster_var) %in% selected_clusters) %>%
                                    tidyr::complete(id, !!sym(cluster_var) := all_clusters, fill = list(count = 0)) %>%
                                    tidyr::fill(!!sym(group), .direction = "downup") %>%
                                    group_by(id) %>%
                                    mutate(prop = count / sum(count) * 100) %>%
                                    ungroup()
    }

    return(cluster_proportions)
}



do_testing <- function(data, grouping_var, module, features, group_by_clusters, cluster_var = cluster_var, selected_clusters = NULL,
                        column_number = 4, parametric_testing = FALSE, paired = FALSE, manual_comparisons = NULL, prefix = NULL) {
  if (is.null(manual_comparisons)) {
    unique_groups <- unique(data[[grouping_var]])
    n_unique_groups <- length(unique(data[[grouping_var]]))
  } else {
    unique_groups <- manual_comparisons
    n_unique_groups <- length(manual_comparisons)
  }

  if (group_by_clusters == TRUE) {
      n_number <- data %>%
                group_by(!!sym(grouping_var), !!sym(cluster_var)) %>%
                summarise(n = n())
  } else {
    n_number <- data %>%
                group_by(!!sym(grouping_var)) %>%
                summarise(n = n())
  }

  min_n_number <- min(n_number$n)

  if (min_n_number < 2) {
    stop("At least two samples in each group are required for testing.")
  }

  temp <- data
  temp[[grouping_var]] <- as.character(temp[[grouping_var]])
  if (n_unique_groups == 2) {
    comparisons <- list(c(unique_groups))

    if (parametric_testing == TRUE) {
      testing_type <- "parametric"

      if (group_by_clusters == TRUE) {
        temp <- temp %>% group_by(!!sym(cluster_var))
      }
      if (!is.null(selected_clusters)) {
          temp <- temp %>% filter(!!sym(cluster_var) %in% selected_clusters)
      }
      collector <- c()
      for (f in seq_along(features)) {
        feature <- features[f]
        test_result <- temp %>% rstatix::t_test(as.formula(paste(feature, "~", grouping_var)), paired = paired, p.adjust.method = "none")
        collector <- rbind(collector, test_result)
      }
      collector <- collector %>% rstatix::adjust_pvalue(method = "BH")
    } else {
      testing_type <- "nonparametric"

      if (group_by_clusters == TRUE) {
        temp <- temp %>% group_by(!!sym(cluster_var))
      }
      if (!is.null(selected_clusters)) {
        temp <- temp %>% filter(!!sym(cluster_var) %in% selected_clusters)
      }
      collector <- c()
      for (f in seq_along(features)) {
        feature <- features[f]
        test_result <- temp %>% rstatix::pairwise_wilcox_test(as.formula(paste(feature, "~", grouping_var)), paired = paired, p.adjust.method = "none")
        collector <- rbind(collector, test_result)
      }
      collector <- collector %>% rstatix::adjust_pvalue(method = "BH")
      collector$p.adj.signif <- symnum(collector$p.adj, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
    }
    
  } else if (n_unique_groups > 2) {
    comparisons <- combn(unique(data[[grouping_var]]), 2, simplify = TRUE)

    if (paired == TRUE) {
      stop("Paired testing is not supported for more than two groups.")
    }

    if (parametric_testing == TRUE) {
        testing_type <- "parametric"

      if (group_by_clusters == TRUE) {
        temp <- temp %>% group_by(!!sym(cluster_var))
      }
      if (!is.null(selected_clusters)) {
        temp <- temp %>% dplyr::filter(!!sym(cluster_var) %in% selected_clusters)
      }
      omnibus_collector <- c()
      collector <- c()
      for (f in seq_along(features)) {
        feature <- features[f]
        omnibus_result <- temp %>% rstatix::anova_test(as.formula(paste(feature, "~", grouping_var)))
        
        if (group_by_clusters == TRUE) {
          signif_clusters <- unlist(omnibus_result[omnibus_result$p < 0.05, cluster_var])
          test_result <- temp %>% dplyr::filter(!!sym(cluster_var) %in% signif_clusters) %>% rstatix::t_test(as.formula(paste(feature, "~", grouping_var)), paired = paired, p.adjust.method = "none")
        } else {
          test_result <- temp %>% rstatix::t_test(as.formula(paste(feature, "~", grouping_var)), paired = paired, p.adjust.method = "none")
        }
        omnibus_collector <- rbind(omnibus_collector, omnibus_result)
        collector <- rbind(collector, test_result)
      }
      write.csv(omnibus_collector, paste0(output_group, prefix, "_", testing_type, "_omnibus_testing_results.csv"))
      collector <- collector %>% rstatix::adjust_pvalue(method = "BH")
      collector$p.adj.signif <- symnum(collector$p.adj, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
    } else {
      testing_type <- "nonparametric"

      if (group_by_clusters == TRUE) {
        temp <- temp %>% group_by(!!sym(cluster_var))
      }
      if (!is.null(selected_clusters)) {
        temp <- temp %>% dplyr::filter(!!sym(cluster_var) %in% selected_clusters)
      }
      omnibus_collector <- c()
      collector <- c()
      for (f in seq_along(features)) {
        feature <- features[f]
        omnibus_result <- temp %>% rstatix::kruskal_test(as.formula(paste(feature, "~", grouping_var)))
        
        if (group_by_clusters == TRUE) {
          signif_clusters <- unlist(omnibus_result[omnibus_result$p < 0.05, cluster_var])
          test_result <- temp %>% dplyr::filter(!!sym(cluster_var) %in% signif_clusters) %>% rstatix::pairwise_wilcox_test(as.formula(paste(feature, "~", grouping_var)), paired = paired, p.adjust.method = "none")
        } else {
          test_result <- temp %>% rstatix::pairwise_wilcox_test(as.formula(paste(feature, "~", grouping_var)), paired = paired, p.adjust.method = "none")
        }
        omnibus_collector <- rbind(omnibus_collector, omnibus_result)
        collector <- rbind(collector, test_result)
      }
      write.csv(omnibus_collector, paste0(output_group, prefix, "_", testing_type, "_omnibus_testing_results.csv"))
      collector <- collector %>% rstatix::adjust_pvalue(method = "BH")
      collector$p.adj.signif <- symnum(collector$p.adj, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
    }
    
  } else if (n_unique_groups < 2) {
    stop("Only one group detected. Please check the data.")
  }
  
  if (paired == TRUE) {
    testing_size <- "pairwise"
  }
  
  write.csv(collector, paste0(output_group, prefix, "_", testing_type, "_pairwise_testing_results.csv"))

  return(collector)
}


