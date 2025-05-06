

if (group == "pre_post") {
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
}

if (project_name == "dc10") {
    if (group != "group") {
        cluster_var <- "meta_cluster_id"
        selected_clusters <- NULL
        grouping_var <- group
        pairing_var <- "donor"
        additional_columns <- c(pairing_var)
        features <- c("prop")
        group_by_clusters <- TRUE
        column_number <- 4
        parametric_testing <- FALSE
        paired <- TRUE
        if (group == "paired_0") {
            manual_comparisons <- c("DC_0h", "DC10_0h")
        } else if (group == "paired_0_5") {
            manual_comparisons <- c("DC_0_5h", "DC10_0_5h")
        } else if (group == "paired_4") {
            manual_comparisons <- c("DC_4h", "DC10_4h")
        } else if (group == "paired_24") {
            manual_comparisons <- c("DC_24h", "DC10_24h")
        } else if (group == "paired_0_LPS") {
            manual_comparisons <- c("DC_LPS_0h", "DC10_LPS_0h")
        } else if (group == "paired_0_5_LPS") {
            manual_comparisons <- c("DC_LPS_0_5h", "DC10_LPS_0_5h")
        } else if (group == "paired_4_LPS") {
            manual_comparisons <- c("DC_LPS_4h", "DC10_LPS_4h")
        } else if (group == "paired_24_LPS") {
            manual_comparisons <- c("DC_LPS_24h", "DC10_LPS_24h")
        }

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


                
        features <- clustering_feature_markers
        prefix <- "marker_expressions_all_clusters_PAIRED"
        


        group_by_clusters <- FALSE
        exprs_averages <- summary_table(data = exprs_set, grouping_var = c(group, "id", "donor"), selected_features = clustering_feature_markers, stat = "mean")
        data <- exprs_averages
        testing_results <- do_testing(data = data, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                        cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                        parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)


        marker_average_heatmaps(data = data, grouping_var = group, features = features, prefix = prefix, pairing_var = pairing_var)


        show_testing <- TRUE
        show_pvalues <- FALSE
        show_outliers <- TRUE
        prefix <- "marker_expressions_all_clusters_w_testing_wo_pvalues_w_outliers_PAIRED"

        do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                    show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                    prefix = prefix)

        show_testing <- TRUE
        show_pvalues <- TRUE
        show_outliers <- TRUE
        prefix <- "marker_expressions_all_clusters_w_testing_w_pvalues_w_outliers_PAIRED"

        do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                    show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                    prefix = prefix)

        show_testing <- TRUE
        show_pvalues <- TRUE
        show_outliers <- FALSE
        prefix <- "marker_expressions_all_clusters_w_testing_w_pvalues_wo_outliers_PAIRED"

        do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                    show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                    prefix = prefix)

    }
}
