
sample_size_bars <- function() {
  count_table <- as.data.frame(table(factor(exprs_set$sample, levels = stringr::str_sort(unique(exprs_set$sample), numeric = TRUE))))
  setwd(output_data_sub_analysis)
  write.csv(x = count_table, file = paste0(data_sub, "_sample_count_table.csv"))

  if (sampling_rate > 1) {
    barcol_palette <- c("no" = "darkorange", "yes" = "darkred")
  }

  pdf(file = paste0(output_data_sub_analysis, "events_per_sample_", date, ".pdf"),
      width = 4,
      height = 2 + (length(unique(exprs_set$sample)) * 0.07))
  print(ggplot(exprs_set,
    aes(x = factor(sample, levels = stringr::str_sort(unique(sample), numeric = TRUE)))) +
    {
      if (sampling_rate > 1) geom_bar(aes(fill = resampled)) else geom_bar(fill = "darkorange")
    } +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 1), hjust = -0.5, size = 1.2) +
    xlab(element_blank()) +
    ylab("Event count") +
    theme_cowplot() +
    theme(
      text = element_text(size = 5),
      axis.text.x = element_text(color = "black", size = 5, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
      axis.text.y = element_text(color = "black", size = 4, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
      plot.margin = margin(t = 0,   # Top margin
                           r = 0,  # Right margin
                           b = 0,  # Bottom margin
                           l = 10)  # Left margin
    ) +
    {
      if (sampling_rate > 1) scale_fill_manual(values = barcol_palette)
    } +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip()
  )
  invisible(dev.off())
}

batch_size_bars <- function() {
  count_table <- as.data.frame(table(factor(exprs_set$batch, levels = stringr::str_sort(unique(exprs_set$batch), numeric = TRUE))))
  setwd(output_data_sub_analysis)
  write.csv(x = count_table, file = paste0(data_sub, "_batch_count_table.csv"))

  if (sampling_rate > 1) {
    barcol_palette <- c("no" = "darkorange", "yes" = "darkred")
  }

  pdf(file = paste0(output_data_sub_analysis, "events_per_batch_", date, ".pdf"),
      width = 4,
      height = 2 + (length(unique(exprs_set$batch)) * 0.07))
  print(ggplot(exprs_set,
    aes(x = factor(batch, levels = stringr::str_sort(unique(batch), numeric = TRUE)))) +
    {
      if (sampling_rate > 1) geom_bar(aes(fill = resampled)) else geom_bar(fill = "darkorange")
    } +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 1), hjust = -0.5, size = 1.2) +
    xlab(element_blank()) +
    ylab("Event count") +
    theme_cowplot() +
    theme(
      text = element_text(size = 5),
      axis.text.x = element_text(color = "black", size = 5, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
      axis.text.y = element_text(color = "black", size = 4, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
      plot.margin = margin(t = 0,   # Top margin
                           r = 0,  # Right margin
                           b = 0,  # Bottom margin
                           l = 10)  # Left margin
    ) +
    {
      if (sampling_rate > 1) scale_fill_manual(values = barcol_palette)
    } +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip()
  )
  invisible(dev.off())
}


make_palette <- function(variable_name) {
  set.seed(1234)
  palette <- Polychrome::createPalette(length(unique(exprs_set[, variable_name])), c("#010101", "#ff0000"), M = 10000)
  names(palette) <- gtools::mixedsort(unique(exprs_set[, variable_name]))
  return(palette)
}



cluster_size_bars <- function() {
  count_table <- as.data.frame(table(factor(exprs_set$meta_cluster_id, levels = stringr::str_sort(unique(exprs_set$meta_cluster_id), numeric = TRUE))))
  setwd(output_clustering)
  write.csv(x = count_table, file = paste0(data_sub, "_cluster_count_table.csv"))

  pdf(file = paste0(output_clustering, "events_per_cluster_", date, ".pdf"),
      width = 4,
      height = 2 + (length(unique(exprs_set$meta_cluster_id)) * 0.07))
  print(ggplot(exprs_set,
    aes(x = factor(meta_cluster_id, levels = stringr::str_sort(unique(meta_cluster_id), numeric = TRUE)))) +
    geom_bar(fill = "darkorange") +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 1), hjust = -0.5, size = 1.2) +
    xlab("Cluster Number") +
    ylab("Event count") +
    theme_cowplot() +
    theme(
      text = element_text(size = 5),
      axis.text.x = element_text(color = "black", size = 5, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
      axis.text.y = element_text(color = "black", size = 4, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
      plot.margin = margin(t = 0,   # Top margin
                           r = 0,  # Right margin
                           b = 0,  # Bottom margin
                           l = 10)  # Left margin
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip()
  )
  invisible(dev.off())
}


cluster_prop_bars <- function() {
  count_table <- as.data.frame(table(factor(exprs_set$meta_cluster_id, levels = stringr::str_sort(unique(exprs_set$meta_cluster_id), numeric = TRUE))))
  setwd(output_clustering)
  write.csv(x = count_table, file = paste0(data_sub, "_cluster_prop_table.csv"))

  pdf(file = paste0(output_clustering, "events_per_cluster_prop_", date, ".pdf"),
      width = 4,
      height = 2 + (length(unique(exprs_set$meta_cluster_id)) * 0.07))
  print(ggplot(exprs_set,
    aes(x = factor(meta_cluster_id, levels = stringr::str_sort(unique(meta_cluster_id), numeric = TRUE)))) +
    geom_bar(fill = "darkorange") +
    geom_text(stat = "count", aes(label = round(after_stat(count) / length(exprs_set$meta_cluster_id) * 100, 2)), position = position_dodge(width = 1), hjust = -0.5, size = 1.2) +
    xlab("Cluster Number") +
    ylab("Percent of total events") +
    theme_cowplot() +
    theme(
      text = element_text(size = 5),
      axis.text.x = element_text(color = "black", size = 5, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
      axis.text.y = element_text(color = "black", size = 4, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
      plot.margin = margin(t = 0,   # Top margin
                           r = 0,  # Right margin
                           b = 0,  # Bottom margin
                           l = 10)  # Left margin
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    coord_flip()
  )
  invisible(dev.off())
}


cluster_expr_heatmap <- function(expression_setting, scale) {
  #exprs_metric can be either means or medians
  if (expression_setting == "means"){
    cluster_matrix <- NULL
    for (i in seq_along(unique(exprs_set$meta_cluster_id))) {
      cluster_matrix <- rbind(cluster_matrix, apply(exprs_set[exprs_set$meta_cluster_id == i, colnames(exprs_set) %in% clustering_feature_markers], FUN = mean, MARGIN = 2))
    }
  }
  if (expression_setting == "medians"){
    cluster_matrix <- NULL
    for (i in seq_along(unique(exprs_set$meta_cluster_id))) {
      cluster_matrix <- rbind(cluster_matrix, miscTools::colMedians(exprs_set[exprs_set$meta_cluster_id == i, colnames(exprs_set) %in% clustering_feature_markers]))
    }
  }

  #cluster rows in the Heatmap if "order" is not set
    if (exists("subset_feature_selection")) {
      row_clust_setting <- FALSE
      cluster_matrix <- cluster_matrix[, clustering_feature_markers]
      col_clust_setting <-  TRUE
    } else {
      row_clust_setting <- TRUE
      col_clust_setting <- TRUE
    }

  if (scale == TRUE) {
    #z-normalize feature expression
    #this is done for better contrast in the heatmap
    cluster_matrix <- apply(cluster_matrix, scale, MARGIN = 2)
  }

  rownames(cluster_matrix) <- paste0("C", seq_len(nrow(cluster_matrix)))

  cluster_cols <- make_palette("meta_cluster_id")
  names(cluster_cols) <- c(paste0("C", names(cluster_cols)))

  if (scale == TRUE) {
  pdf(file = paste0(output_clustering, "heatmap_cluster_expr_", expression_setting, "_scaled", ".pdf"), width = 0.85 * (nrow(cluster_matrix)), height = 0.7 * (length(clustering_feature_markers)))
  } else {
  pdf(file = paste0(output_clustering, "heatmap_cluster_expr_", expression_setting, ".pdf"), width = 0.85 * (nrow(cluster_matrix)), height = 0.7 * (length(clustering_feature_markers)))
  }
  column_ha <- HeatmapAnnotation(cluster = rownames(cluster_matrix),
                                  annotation_legend_param = list(cluster = list(ncol = 2, 
                                                                                title = "Cluster",
                                                                                title_position = "topcenter",
                                                                                at = gtools::mixedsort(names(cluster_cols)),
                                                                                grid_height = unit(0.02 * length(feature_markers), "cm"),
                                                                                grid_width = unit(0.02 * length(feature_markers), "cm"),
                                                                                labels_gp = gpar(fontsize = 0.8 * length(feature_markers)),
                                                                                title_gp = gpar(fontsize = 0.8 * length(feature_markers))
                                                                                )
                                                                ),
                                  col = list(cluster = cluster_cols),
                                  na_col = "white",
                                  show_annotation_name = FALSE
                                 )  
  hm <- Heatmap(t(cluster_matrix),
                cluster_rows = row_clust_setting,
                cluster_columns = col_clust_setting,
                row_names_gp = gpar(fontsize = 0.8 * length(feature_markers)),
                column_names_gp = gpar(fontsize = 0.8 * length(feature_markers)),# Text size for row names
                top_annotation = column_ha,
                heatmap_legend_param = list(title = "Scaled expression",
                                            direction = "horizontal",
                                            title_position = "topcenter",
                                            legend_width = unit(0.25 * length(feature_markers), "cm"),
                                            grid_width = unit(0.02 * length(feature_markers), "cm"),
                                            labels_gp = gpar(fontsize = 0.8 * length(feature_markers)),
                                            title_gp = gpar(fontsize = 0.8 * length(feature_markers))
                                          )
                )
  draw(hm,
        heatmap_legend_side = "top",
        row_sub_title_side = "left",
        padding = unit(c(3, 2, 2, 15), "mm"))
  invisible(dev.off())
}


cluster_expr_densities <- function() {
  col_number <- 5
  pdf(file = paste0(output_clustering, "density_cluster_exprs.pdf"), width = 4 * col_number, height = 3 * ceiling(length(clustering_feature_markers) / col_number))
  for (i in seq_along(unique(exprs_set$meta_cluster_id))) {
    gg_df <- tidyr::pivot_longer(exprs_set[exprs_set$meta_cluster_id == i, c(clustering_feature_markers)], cols = all_of(clustering_feature_markers), names_to = "antigen") %>%
              mutate(antigen = factor(antigen, levels = c(clustering_feature_markers), ordered = TRUE))
    print(
      ggplot(gg_df, aes(x = value, y = after_stat(ndensity))) + 
            facet_wrap(~ antigen, scales = "free_x", ncol = col_number) +
            geom_density(fill = "darkorange") + 
            ylab("Normalized density") +
            ggtitle(paste0("Cluster ", i)) +
            theme_cowplot() + theme(
                panel.grid = element_blank(), 
                legend.position = "none",
                strip.background = element_blank(),
                strip.text = element_text(face = "bold"),
                axis.text = element_text(color = "black"), 
                axis.title = element_text(color = "black"))
    )
  }
  dev.off()
}
