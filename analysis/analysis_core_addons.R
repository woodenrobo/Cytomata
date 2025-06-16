

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
                prefix = prefix, remove_unpaired = TRUE)


    show_testing <- TRUE
    show_pvalues <- TRUE
    show_outliers <- TRUE
    prefix <- "meta_cluster_id_prop_all_clusters_w_testing_w_pvalues_w_outliers_PAIRED"

    do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                prefix = prefix, remove_unpaired = TRUE)


    show_testing <- TRUE
    show_pvalues <- TRUE
    show_outliers <- FALSE
    prefix <- "meta_cluster_id_prop_all_clusters_w_testing_w_pvalues_wo_outliers_PAIRED"

    do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                prefix = prefix, remove_unpaired = TRUE)







    
    group_by_clusters <- FALSE
    exprs_averages <- summary_table(data = exprs_set, grouping_var = c(grouping_var, "id", pairing_var), selected_features = clustering_feature_markers, stat = "mean", prefix = prefix)
    data <- exprs_averages
    testing_results <- do_testing(data = data, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                    parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)


    
    show_testing <- TRUE
    show_pvalues <- FALSE
    show_outliers <- TRUE
    prefix <- "marker_expressions_all_clusters_w_testing_wo_pvalues_w_outliers_PAIRED"

    do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                prefix = prefix, remove_unpaired = TRUE)

    show_testing <- TRUE
    show_pvalues <- TRUE
    show_outliers <- TRUE
    prefix <- "marker_expressions_all_clusters_w_testing_w_pvalues_w_outliers_PAIRED"

    do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                prefix = prefix, remove_unpaired = TRUE)

    show_testing <- TRUE
    show_pvalues <- TRUE
    show_outliers <- FALSE
    prefix <- "marker_expressions_all_clusters_w_testing_w_pvalues_wo_outliers_PAIRED"

    do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                prefix = prefix, remove_unpaired = TRUE)



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
        parametric_testing <- TRUE
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
                    prefix = prefix, remove_unpaired = TRUE)


        show_testing <- TRUE
        show_pvalues <- TRUE
        show_outliers <- TRUE
        prefix <- "meta_cluster_id_prop_all_clusters_w_testing_w_pvalues_w_outliers_PAIRED"

        do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                    show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                    prefix = prefix, remove_unpaired = TRUE)


        show_testing <- TRUE
        show_pvalues <- TRUE
        show_outliers <- FALSE
        prefix <- "meta_cluster_id_prop_all_clusters_w_testing_w_pvalues_wo_outliers_PAIRED"

        do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                    show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                    prefix = prefix, remove_unpaired = TRUE)


                
        features <- clustering_feature_markers
        prefix <- "marker_expressions_all_clusters_PAIRED"
        


        group_by_clusters <- FALSE
        exprs_averages <- summary_table(data = exprs_set, grouping_var = c(group, "id", "donor"), selected_features = clustering_feature_markers, stat = "mean", prefix = prefix)
        data <- exprs_averages
        testing_results <- do_testing(data = data, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                        cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                        parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)



        custom_order <- c("pSTAT1_Y701",
            "pSTAT1_S727",
            "pSTAT2_Y689",
            "pSTAT3_Y705",
            "pSTAT3_S727",
            "pSTAT5_T694",
            "pRelA_S536",
            "pAMPKa_T172",
            "pPDK1_S241",
            "panAkt",
            "pAkt_S473",
            "pAKT_T308",
            "p_mTOR_S2448",
            "p_p70S6K_T389",
            "p4eBP1_T37_T46",
            "pS6_S235_S236",
            "HIF1a",
            "pGSK3b_S9",
            "pMEK1_2_S217_S221",
            "pERK1_2_T202_Y204",
            "p_p90RSK_S380",
            "pMKK3_6_S189_S207",
            "p_p38_T180_Y182",
            "pMAPKAPK2_T334",
            "CDT1_DUP",
            "Geminin",
            "CyclinD1",
            "pRB_S807_S811",
            "CyclinB1",
            "pH3_S28",
            "Ki67",
            "cCasp3",
            "pSRC_T418",
            "pCREB"
        )

        marker_average_heatmaps(data = data, grouping_var = group, features = features, prefix = prefix,
                                pairing_var = pairing_var,
                                custom_marker_order = custom_order)



        show_testing <- TRUE
        show_pvalues <- FALSE
        show_outliers <- TRUE
        prefix <- "marker_expressions_all_clusters_w_testing_wo_pvalues_w_outliers_PAIRED"

        do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                    show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                    prefix = prefix, remove_unpaired = TRUE)

        show_testing <- TRUE
        show_pvalues <- TRUE
        show_outliers <- TRUE
        prefix <- "marker_expressions_all_clusters_w_testing_w_pvalues_w_outliers_PAIRED"

        do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                    show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                    prefix = prefix, remove_unpaired = TRUE)

        show_testing <- TRUE
        show_pvalues <- TRUE
        show_outliers <- FALSE
        prefix <- "marker_expressions_all_clusters_w_testing_w_pvalues_wo_outliers_PAIRED"

        do_paired_boxplots(data = data, testing_results = testing_results, grouping_var = grouping_var, pairing_var = pairing_var, features = features, group_by_clusters = group_by_clusters,
                    cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                    show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers,
                    prefix = prefix, remove_unpaired = TRUE)

    }
}
