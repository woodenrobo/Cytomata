
cluster_var <- "meta_cluster_id"
selected_clusters <- NULL
grouping_var <- group
pairing_var <- "patient"
additional_columns <- c(pairing_var)
features <- c("prop")
group_by_clusters <- TRUE
column_number <- 4
parametric_testing <- FALSE
paired <- TRUE
manual_comparisons <- c("S1.1", "S1.2")


prefix <- "meta_cluster_id_prop_all_clusters_PAIRED"

cluster_proportions <- calculate_cluster_proportions(cluster_var = cluster_var, additional_columns = additional_columns, selected_clusters = selected_clusters, prefix = prefix)
data <- cluster_proportions
data <- data[unlist(data[, grouping_var]) %in% manual_comparisons, ]
testing_results <- do_testing(data = data, grouping_var = grouping_var, features = features, group_by_clusters = group_by_clusters,
                                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)


show_testing <- TRUE
show_pvalues <- FALSE
show_outliers <- TRUE
prefix <- "meta_cluster_id_prop_all_clusters_w_testing_wo_pvalues_w_outliers_PAIRED"

do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
            show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
            prefix = prefix)


show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- TRUE
prefix <- "meta_cluster_id_prop_all_clusters_w_testing_w_pvalues_w_outliers_PAIRED"

do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
            show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
            prefix = prefix)


show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- FALSE
prefix <- "meta_cluster_id_prop_all_clusters_w_testing_w_pvalues_wo_outliers_PAIRED"

do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
            show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
            prefix = prefix)