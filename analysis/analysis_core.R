summary_table(exprs_set, c(group), selected_features = NULL, "n_size")
summary_table(exprs_set, c(group, "id"), selected_features = clustering_feature_markers, "mean")
summary_table(exprs_set, c(group, "id"), selected_features = clustering_feature_markers, "median")

cluster_var <- "meta_cluster_id"
calculate_cluster_proportions(cluster_var = cluster_var, selected_clusters = unique(exprs_set[[cluster_var]]))
cluster_var <- "meta_cluster_annotation"
calculate_cluster_proportions(cluster_var = cluster_var, selected_clusters = unique(exprs_set[[cluster_var]]))