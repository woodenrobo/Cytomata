
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



cluster_size_bars <- function(after_dropping = FALSE) {
  count_table <- as.data.frame(table(factor(exprs_set$meta_cluster_id, levels = stringr::str_sort(unique(exprs_set$meta_cluster_id), numeric = TRUE))))
  setwd(output_clustering)
  if (after_dropping == TRUE) {
    write.csv(x = count_table, file = paste0(data_sub, "_cluster_count_table_DROPPED_EVENTS.csv"))
  } else {
    write.csv(x = count_table, file = paste0(data_sub, "_cluster_count_table.csv"))
  }
  
  if (after_dropping == TRUE) {
    pdf(file = paste0(output_clustering, "events_per_cluster_", date, "_DROPPED_EVENTS.pdf"),
      width = 4,
      height = 2 + (length(unique(exprs_set$meta_cluster_id)) * 0.07))
  } else {
    pdf(file = paste0(output_clustering, "events_per_cluster_", date, ".pdf"),
      width = 4,
      height = 2 + (length(unique(exprs_set$meta_cluster_id)) * 0.07))
  }

  
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


cluster_prop_bars <- function(after_dropping = FALSE) {
  count_table <- as.data.frame(table(factor(exprs_set$meta_cluster_id, levels = stringr::str_sort(unique(exprs_set$meta_cluster_id), numeric = TRUE))))
  setwd(output_clustering)
  
  if (after_dropping == TRUE) {
    write.csv(x = count_table, file = paste0(data_sub, "_cluster_prop_table_DROPPED_EVENTS.csv"))
  } else {
    write.csv(x = count_table, file = paste0(data_sub, "_cluster_prop_table.csv"))
  }
  
  if (after_dropping == TRUE) {
    pdf(file = paste0(output_clustering, "events_per_cluster_prop_", date, "_DROPPED_EVENTS.pdf"),
      width = 4,
      height = 2 + (length(unique(exprs_set$meta_cluster_id)) * 0.07))
  } else {
    pdf(file = paste0(output_clustering, "events_per_cluster_prop_", date, ".pdf"),
      width = 4,
      height = 2 + (length(unique(exprs_set$meta_cluster_id)) * 0.07))
  }

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


cluster_expr_heatmap <- function(expression_setting, scale, after_dropping = FALSE) {
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
  if (after_dropping == TRUE){
    if (scale == TRUE) {
      pdf(file = paste0(output_clustering, "heatmap_cluster_expr_", expression_setting, "_scaled_DROPPED_EVENTS", ".pdf"), width = 1 * (nrow(cluster_matrix)), height = 0.7 * (length(clustering_feature_markers)))
    } else {
      pdf(file = paste0(output_clustering, "heatmap_cluster_expr_", expression_setting, "_DROPPED_EVENTS.pdf"), width = 1 * (nrow(cluster_matrix)), height = 0.7 * (length(clustering_feature_markers)))
    }
  } else {
    if (scale == TRUE) {
      pdf(file = paste0(output_clustering, "heatmap_cluster_expr_", expression_setting, "_scaled", ".pdf"), width = 1 * (nrow(cluster_matrix)), height = 0.7 * (length(clustering_feature_markers)))
    } else {
      pdf(file = paste0(output_clustering, "heatmap_cluster_expr_", expression_setting, ".pdf"), width = 1 * (nrow(cluster_matrix)), height = 0.7 * (length(clustering_feature_markers)))
    }
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


cluster_expr_densities <- function(after_dropping = FALSE) {
  col_number <- 5
  
  if (after_dropping == TRUE) {
    pdf(file = paste0(output_clustering, "density_cluster_exprs_DROPPED_EVENTS.pdf"), width = 4 * col_number, height = 3 * ceiling(length(clustering_feature_markers) / col_number))
  } else {
    pdf(file = paste0(output_clustering, "density_cluster_exprs.pdf"), width = 4 * col_number, height = 3 * ceiling(length(clustering_feature_markers) / col_number))
  }

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


pca_biplot <- function(grouping_var, dims, module) {
  options(warn = 1)
  averaged_pca <- c()
  for (i in unique(exprs_set[, grouping_var])) {
    temp1 <- apply(exprs_set[exprs_set[, grouping_var] == i, paste0("PC", rep(dims))], FUN = mean, MARGIN = 2)
    temp2 <- unique(as.character(exprs_set[exprs_set[, grouping_var] == i, grouping_var]))
    temp1$group <- temp2
    temp1 <- as.data.frame(temp1)
    averaged_pca <- rbind(averaged_pca, temp1)
  }

  temp_pca <- pca
  # temp_pca$sdev <- pca$sdev[dims]
  temp_pca$rotation <- pca$rotation[, dims]
  temp_pca$x <- as.matrix(averaged_pca[, paste0("PC", rep(dims))])
  
  cols <- make_palette(grouping_var)
  if (module == "exploration"){
    folder <- output_exploration
  } else if (module == "core"){
    folder <- output_core
  }
  pdf(paste0(folder, "PCA_", grouping_var, "_PC_", paste0(dims, collapse = "_"), ".pdf"),
    width = 13,
    height = 10
  )

  print(autoplot(temp_pca, data = averaged_pca["group"], color = "group",
               x = dims[1],
               y = dims[2],
               loadings = TRUE, loadings.colour = 'black',
               loadings.label.colour = 'black', loadings.label = TRUE, loadings.label.size = 10,
               loadings.label.repel = TRUE) +
        stat_ellipse(type = "norm", level = 0.68, size = 2, alpha = 0.8, aes(color = group, fill = group)) +
        stat_ellipse(geom = "polygon", alpha = 0.3, aes(fill = group)) +
        geom_point(aes(color = group), size = 3.5, alpha = 1,) +
        theme_cowplot() +
        scale_color_manual(values = cols, limits = force, labels = scales::label_wrap(25)) +
        labs(color = grouping_var)+ 
        theme(text=element_text(size=25),
              plot.title = element_text(size=20),
              #legend.position="none",
              axis.text.x = element_text(color = "black", size = 20
                                         , angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.text.y = element_text(color = "black", size = 20
                                         , angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.title.x.bottom = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                                 size = 25),
              axis.title.y.left = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                               size = 25)
        )
  )
  invisible(dev.off())

}


get_contrasting_text_color <- function(hex_color) {
  rgb <- col2rgb(hex_color) / 255
  luminance <- unname(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
  ifelse(luminance > 0.5, "black", "white")
}


umap_plot <- function(grouping_var, module, labels = TRUE) {
  cols <- make_palette(grouping_var)
  text_colors <- sapply(cols, get_contrasting_text_color)

  if (module == "exploration"){
    folder <- output_exploration
  } else if (module == "core"){
    folder <- output_core
  }

  pdf(paste0(folder, "UMAP_", grouping_var, ".pdf"),
    width = 13,
    height = 10
  )

  p <- ggplot(exprs_set, aes(x = UMAP1, y = UMAP2)) +
        rasterise(geom_point(aes(color = as.factor(.data[[grouping_var]])), alpha = 0.5, size = 1, shape = 19)) +
        # size = 0.5 to restore old version with big points 
        # shape = "." to optimize for execution speed
        guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3, shape = 19), title = grouping_var)) +
        scale_color_manual(values = cols, labels = scales::label_wrap(25)) +
        theme_cowplot() +
        theme(text = element_text(size = 25),
              axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
              axis.title.y = element_text(margin = margin(0, 10, 0, 0))
        )

  if (labels == TRUE) {
    # Calculate mean coordinates for each group
    mean_coords <- exprs_set %>%
      group_by(.data[[grouping_var]]) %>%
      summarize(UMAP1 = mean(UMAP1), UMAP2 = mean(UMAP2))
    p <- p + 
          ggnewscale::new_scale_color() +
          geom_label_repel(data = mean_coords, 
            aes(label = str_wrap(.data[[grouping_var]], width = 25),
             fill = as.factor(.data[[grouping_var]]),
             color = as.factor(mean_coords[[grouping_var]])),
            size = 10, max.overlaps = Inf) +
          scale_fill_manual(values = cols) +
          scale_color_manual(values = text_colors) +
          guides(fill = "none", color = "none")
  }
  print(p)
  invisible(dev.off())


}


umap_facet <- function(grouping_var, module, column_number = 4, equal_sampling = FALSE) {
  cols <- make_palette(grouping_var)

  if (module == "exploration") {
    folder <- output_exploration
  } else if (module == "core") {
    folder <- output_core
  }

  singles_output <- paste0(folder, "UMAP_facet_", grouping_var, "/")
  dir.create(singles_output, showWarnings = FALSE)

  if (equal_sampling == TRUE) {
    max_equal_sampling <- min(table(exprs_set[[grouping_var]]))
    plot_df  <- exprs_set %>% group_by(.data[[grouping_var]]) %>% slice_sample(n = max_equal_sampling)
    plot_df[[grouping_var]] <- factor(plot_df[[grouping_var]])
  } else {
    plot_df <- exprs_set
  }

  grouping_levels <- levels(plot_df[[grouping_var]])

  p <- list()
  for (s in seq(grouping_levels)) {
    plotted_group <- grouping_levels[s]
    p[[s]] <- ggplot(plot_df[plot_df[[grouping_var]] == plotted_group, ], aes(x = UMAP1, y = UMAP2)) +
      rasterise(geom_point(data = plot_df, color = '#aeaeae', alpha = 0.5, size = 1, shape = 19)) + 
      rasterise(geom_point(aes(color = !!sym(grouping_var)), alpha = 0.5, size = 1, shape = 19, show.legend = F)) + 
      scale_color_manual(values = cols) +
      ggtitle(paste(plotted_group)) + 
      theme_cowplot() +
      theme(text = element_text(size = 25),
            axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
            axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
            axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
            axis.title.y = element_text(margin = margin(0, 10, 0, 0))
      )
    pdf(paste0(singles_output, "UMAP_", grouping_var, "_", plotted_group, ".pdf"),
    width = 11,
    height = 10
    )
  print(p[[s]])
  invisible(dev.off())
  }

  pdf(paste0(folder, "UMAP_facet_", grouping_var, ".pdf"),
    width = 11 * column_number,
    height = 10 * ceiling(length(grouping_levels) / column_number)
  )
  print(plot_grid(plotlist = p, ncol = column_number))
  invisible(dev.off())

}


umap_expressions <- function(grouping_var = NULL, module, column_number = 4) {
  
  if (module == "exploration") {
    folder <- output_exploration
  } else if (module == "core") {
    folder <- output_core
  }

  singles_output <- paste0(folder, "UMAP_expressions/")
  dir.create(singles_output, showWarnings = FALSE)

  plot_df <- exprs_set

  p <- list()
  for (s in seq(clustering_feature_markers)) {
    plotted_marker <- clustering_feature_markers[s]
    p[[s]] <- ggplot(plot_df %>% arrange(!!sym(plotted_marker)), aes(x = UMAP1, y = UMAP2)) +
      rasterise(geom_point(aes(color = !!sym(plotted_marker)), alpha = 1, size = 1, shape = 19)) + 
      scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      ggtitle(paste(plotted_marker)) + 
      theme_cowplot() +
      theme(text = element_text(size = 25),
            axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
            axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
            axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
            axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
            legend.key.size = unit(3, "lines"),
            panel.background = element_rect(fill = "#bdbdbd", color = "black"),  # Background color of the plot area
            plot.background = element_rect(fill = "#ffffff", color = NA)
      )
    
    if (!is.null(grouping_var)) {
      plot_df[[grouping_var]] <- factor(plot_df[[grouping_var]])
      grouping_levels <- levels(plot_df[[grouping_var]])

      p[[s]] <- p[[s]] +
                  facet_wrap(~ .data[[grouping_var]], ncol = column_number) +
                  theme(strip.background = element_rect(fill = "#ffffff"))
      
      if (length(grouping_levels) < column_number) {
        column_n_single <- length(grouping_levels)
      } else {
        column_n_single <- column_number
      }
      pdf(paste0(singles_output, "UMAP_", grouping_var, "_", plotted_marker, ".pdf"),
        width = 11 * column_n_single,
        height = 10 * ceiling(length(grouping_levels) / column_n_single)
      )
    } else {
      pdf(paste0(singles_output, "UMAP_", plotted_marker, ".pdf"),
        width = 11,
        height = 10
      )
    }

    print(p[[s]])
    invisible(dev.off())
  }


  if (!is.null(grouping_var)) {
    pdf(paste0(folder, "UMAP_expressions_", grouping_var, ".pdf"),
      width = 11 * column_number * 2,
      height = 10 * ceiling(length(clustering_feature_markers) / column_number)
    )


  } else {
    pdf(paste0(folder, "UMAP_expressions.pdf"),
      width = 11 * column_number,
      height = 10 * ceiling(length(clustering_feature_markers) / column_number)
    )
  }

  print(plot_grid(plotlist = p, ncol = column_number))
  invisible(dev.off())

}
