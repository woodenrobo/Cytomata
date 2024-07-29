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
    temp <- left_join(exprs_set, cluster_ids, by = "cell_id")
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
        update_settings()
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


    skip_or_merge_and_annotate()
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
        apply_annotation(cluster_annot)
        merge_or_delete_clusters(cluster_annot)
    } else if (answer == "skip") {
        cat("Continuing without merging, deleting or annotating\n")
    } else {
        cat("\n\nIt seems you have typed an incorrect answer!\n\n")
        skip_or_merge_and_annotate()
    }

    if (first_run_mode < 1) {
        cluster_annot <- readxl::read_excel(paste0(output_data_sub, "cluster_merging_and_annotation.xlsx"))[-1]
        cat("Cluster annotations, merging and deletion parameters have been applied, if set.\n")
        apply_annotation()
        merge_or_delete_clusters()
    }
    answer <- NULL
}


apply_annotation <- function(cluster_annot) {
    for (i in cluster_annot$original_clusters) {
        exprs_set$meta_cluster_annotation[exprs_set$meta_cluster_id == i] <- cluster_annot[cluster_annot$original_clusters == i, "annotation"]
    }
}


merge_or_delete_clusters <- function(cluster_annot) {
    rev_cluster_annot <- cluster_annot[sort(cluster_annot$original_clusters, decreasing = TRUE), ]
    rev_cluster_annot$merge_with <- as.numeric(rev_cluster_annot$merge_with)
    #exprs_set$temp <- exprs_set$meta_cluster_id
    for (i in rev_cluster_annot$original_clusters) {
        from <- i
        to <- unlist(rev_cluster_annot[rev_cluster_annot$original_clusters == i, "merge_with"])
        if (is.na(to) == FALSE && to != 0) {
            exprs_set$meta_cluster_id[exprs_set$meta_cluster_id == to | exprs_set$meta_cluster_id == from] <- to
            exprs_set$meta_cluster_id[exprs_set$meta_cluster_id > from] <- exprs_set$meta_cluster_id[exprs_set$meta_cluster_id > from] - 1
        } else if (is.na(to) == FALSE && to == 0) {
            exprs_set$meta_cluster_id[exprs_set$meta_cluster_id == from] <- to
            exprs_set$meta_cluster_id[exprs_set$meta_cluster_id > from] <- exprs_set$meta_cluster_id[exprs_set$meta_cluster_id > from] - 1           
        }
    }
    if (sum(grepl("^0$", exprs_set$meta_cluster_id)) > 0) {
        dropped_events <<- exprs_set$cell_id[grepl("^0$", exprs_set$meta_cluster_id)]
    }

}

do_pca <- function() {
    pca <- prcomp(exprs_set[, colnames(exprs_set) %in% clustering_feature_markers], scale. = FALSE)
    pca_coords <- as.data.frame(pca$x)
    pca_coords$cell_id <- exprs_set$cell_id

    return(pca_coords)
}

merge_exprs_and_pca <- function() {
    temp <- left_join(exprs_set, pca_coords[, c("PC1", "PC2", "PC3", "PC4", "cell_id")], by = "cell_id")
    return(temp)
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
    temp <- left_join(exprs_set, umap_coords, by = "cell_id")
    return(temp)
}


remove_dropped_events <- function() {
    if (length(dropped_events) > 0) {
        temp <- exprs_set[!exprs_set$cell_id %in% dropped_events, ]
        cat(paste0(length(dropped_events)," events from deleted clusters removed\n"))
    }

    if (dropped_samples_mode > 0) {
        temp <- temp[!temp$sample_state == "dropped", ]
        cat(paste0(sum(exprs_set$sample_state == "dropped"), " events from deleted samples removed\n"))
    }

    return(temp)
}






do_umap_diagnostics <- function() {



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