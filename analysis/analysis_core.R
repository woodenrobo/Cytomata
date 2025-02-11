write.csv(summary_table(data = exprs_set, grouping_var = c(group), selected_features = NULL, stat = "n_size"),
          file = paste0(output_group, "summary_table_group_size.csv"), row.names = FALSE)



cluster_var <- "meta_cluster_id"
selected_clusters <- NULL
grouping_var <- group
features <- c("prop")
group_by_clusters <- TRUE
column_number <- 4
parametric_testing <- FALSE
paired <- FALSE
manual_comparisons <- NULL


prefix <- "meta_cluster_id_prop_all_clusters"

cluster_proportions <- calculate_cluster_proportions(cluster_var = cluster_var, selected_clusters = selected_clusters, prefix = prefix)
data <- cluster_proportions
testing_results <- do_testing(data = data, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)

cluster_abundance_heatmaps(data = data, grouping_var = group, features = features, cluster_var = cluster_var, selected_clusters = selected_clusters, prefix = prefix)

cluster_testing_heatmaps(data = data, testing_results = testing_results, grouping_var = group, features = features, 
                            cluster_var = cluster_var, selected_clusters = selected_clusters, prefix = prefix)

prefix <- "meta_cluster_id_prop_all_clusters_DENDROGRAM_ORDER"
cluster_abundance_heatmaps(data = data, grouping_var = group, features = features, cluster_var = cluster_var, selected_clusters = selected_clusters, cluster_ordering = TRUE, prefix = prefix)

cluster_testing_heatmaps(data = data, testing_results = testing_results, grouping_var = group, features = features, 
                            cluster_var = cluster_var, selected_clusters = selected_clusters, cluster_ordering = TRUE, prefix = prefix)



show_testing <- TRUE
show_pvalues <- FALSE
show_outliers <- TRUE
prefix <- "meta_cluster_id_prop_all_clusters_w_testing_wo_pvalues_w_outliers"

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
            show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
            prefix = prefix)


show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- TRUE
prefix <- "meta_cluster_id_prop_all_clusters_w_testing_w_pvalues_w_outliers"

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
             show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
            prefix = prefix)


show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- FALSE
prefix <- "meta_cluster_id_prop_all_clusters_w_testing_w_pvalues_wo_outliers"

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
            show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
            prefix = prefix)


if (sum(colnames(exprs_set) %in% c("meta_cluster_annotation")) > 0 && sum(exprs_set$meta_cluster_annotation != "NA") > 0) {
    cluster_var <- "meta_cluster_annotation"
    selected_clusters <- NULL
    grouping_var <- group
    features <- c("prop")
    group_by_clusters <- TRUE
    column_number <- 4
    parametric_testing <- FALSE
    paired <- FALSE
    manual_comparisons <- NULL
    prefix <- "meta_cluster_annotation_prop_all_clusters"

    cluster_proportions <- calculate_cluster_proportions(cluster_var = cluster_var, selected_clusters = selected_clusters, prefix = prefix)
    data <- cluster_proportions
    testing_results <- do_testing(data = data, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                    parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)

    cluster_abundance_heatmaps(data = data, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number, prefix = prefix)

    cluster_testing_heatmaps(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                cluster_var = cluster_var, selected_clusters = selected_clusters, prefix = prefix)


    show_testing <- TRUE
    show_pvalues <- FALSE
    show_outliers <- TRUE
    prefix <- "meta_cluster_annotation_prop_all_clusters_w_testing_wo_pvalues_w_outliers"

    do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                prefix = prefix)

    show_testing <- TRUE
    show_pvalues <- TRUE
    show_outliers <- TRUE
    prefix <- "meta_cluster_annotation_prop_all_clusters_w_testing_w_pvalues_w_outliers"

    do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                prefix = prefix)


    show_testing <- TRUE
    show_pvalues <- TRUE
    show_outliers <- FALSE
    prefix <- "meta_cluster_annotation_prop_all_clusters_w_testing_w_pvalues_wo_outliers"

    do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                prefix = prefix)
}



selected_clusters <- NULL
grouping_var <- group
features <- clustering_feature_markers
group_by_clusters <- FALSE
column_number <- 4
parametric_testing <- FALSE
paired <- FALSE
manual_comparisons <- NULL
prefix <- "marker_expressions_all_clusters"
exprs_averages <- summary_table(data = exprs_set, grouping_var = c(group, "id"), selected_features = clustering_feature_markers, stat = "mean")
data <- exprs_averages
testing_results <- do_testing(data = data, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)


marker_average_heatmaps(data = data, grouping_var = group, features = features, prefix = prefix)


show_testing <- TRUE
show_pvalues <- FALSE
show_outliers <- TRUE
prefix <- "marker_expressions_all_clusters_w_testing_wo_pvalues_w_outliers"

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
            show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
            prefix = prefix)

show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- TRUE
prefix <- "marker_expressions_all_clusters_w_testing_w_pvalues_w_outliers"

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
            show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
            prefix = prefix)

show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- FALSE
prefix <- "marker_expressions_all_clusters_w_testing_w_pvalues_wo_outliers"

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
            show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
            prefix = prefix)

do_umap_plots(module = "core")


