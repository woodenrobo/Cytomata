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

show_testing <- TRUE
show_pvalues <- FALSE
show_outliers <- TRUE
prefix <- "meta_cluster_id"

cluster_proportions <- calculate_cluster_proportions(cluster_var = cluster_var, selected_clusters = selected_clusters)

testing_results <- do_testing(data = cluster_proportions, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)

do_boxplots(data = cluster_proportions, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number, show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers)

show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- TRUE

do_boxplots(data = cluster_proportions, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number, show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers)


show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- FALSE

do_boxplots(data = cluster_proportions, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number, show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers)



cluster_var <- "meta_cluster_annotation"
selected_clusters <- NULL
grouping_var <- group
features <- c("prop")
group_by_clusters <- TRUE
column_number <- 4
parametric_testing <- FALSE
paired <- FALSE
manual_comparisons <- NULL

show_testing <- TRUE
show_pvalues <- FALSE
show_outliers <- TRUE
prefix <- "meta_cluster_annotation"

cluster_proportions <- calculate_cluster_proportions(cluster_var = cluster_var, selected_clusters = selected_clusters)
data <- cluster_proportions
testing_results <- do_testing(data = data, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number, show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers)

show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- TRUE

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number, show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers)


show_testing <- TRUE
show_pvalues <- TRUE
show_outliers <- FALSE

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number, show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers)




cluster_var <- "meta_cluster_annotation"
selected_clusters <- NULL
grouping_var <- group
features <- clustering_feature_markers
group_by_clusters <- FALSE
column_number <- 4
parametric_testing <- FALSE
paired <- FALSE
manual_comparisons <- NULL

exprs_averages <- summary_table(data = exprs_set, grouping_var = c(group, "id"), selected_features = clustering_feature_markers, stat = "mean")
data <- exprs_averages
testing_results <- do_testing(data = data, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
                                cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number,
                                parametric_testing = parametric_testing, paired = paired, manual_comparisons = manual_comparisons, prefix = prefix)

#ACHTUNG FOR TESTING ONLY DELETE ME!!!!!************************************************************************
testing_results$p.adj <- rnorm(nrow(testing_results), 0.05, sd = 0.01)
testing_results$p.adj.signif <- symnum(testing_results$p.adj, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))

show_testing <- TRUE
show_pvalues <- FALSE
show_outliers <- TRUE
prefix <- "marker_expression"

do_boxplots(data = data, testing_results = testing_results, grouping_var = group, features = features, group_by_clusters = group_by_clusters,
            cluster_var = cluster_var, selected_clusters = selected_clusters, column_number = column_number, show_testing = show_testing, show_pvalues = show_pvalues, show_outliers = show_outliers)

do_umap_plots(module = "core")


