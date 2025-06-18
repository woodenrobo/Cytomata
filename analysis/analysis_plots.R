
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
  palette <- palette[1:length(unique(exprs_set[, variable_name]))]
  names(palette) <- gtools::mixedsort(unique(exprs_set[, variable_name]))
  return(palette)
}


make_palette_groups <- function(variable_name, add_luminance = -1.2) {

  set.seed(1234)
  palette <- Polychrome::createPalette(length(levels(exprs_set[, variable_name])), c("#b55555", "#4080ab", "#9b4f89"), M = 10000)
  palette <- palette[1:length(levels(exprs_set[, variable_name]))]

  if (-2 < add_luminance && add_luminance < 2) {
    # Get a matrix of values in the RGB color space
    rgb_matrix <- t(grDevices::col2rgb(palette, alpha = TRUE)) / 255
    # Obtain the alpha values
    alpha <- rgb_matrix[, "alpha"]
    # Get a matrix of values in the Luv color space
    luv_matrix <- grDevices::convertColor(rgb_matrix[, 1:3], "sRGB", "Luv")
    # Apply calculations to obtain values in the HCL color space
    h <- atan2(luv_matrix[, "v"], luv_matrix[, "u"]) * 180 / pi
    c <- sqrt(luv_matrix[, "u"]^2 + luv_matrix[, "v"]^2)
    l <- luv_matrix[, "L"]
    # Scale luminance to occupy [0, 1]
    y <- l / 100.
    # Obtain `x` positions of luminance values along a sigmoid function
    x <- log(-(y / (y - 1)))
    # Calculate new luminance values based on a fixed step-change in `x`
    y_2 <- 1 / (1 + exp(-(x + add_luminance)))
    # Rescale the new luminance values to [0, 100]
    l <- y_2 * 100.
    # Obtain hexadecimal colors from the modified HCL color values
    palette <- grDevices::hcl(h, c, l, alpha = alpha)
  } else {
    warning("add_luminance must be between -2 and 2, ignoring the parameter")
  }

  names(palette) <- gtools::mixedsort(levels(exprs_set[, variable_name]))

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
  if (expression_setting == "means") {
    cluster_matrix <- NULL
    for (i in seq_along(unique(exprs_set$meta_cluster_id))) {
      cluster_matrix <- rbind(cluster_matrix, apply(exprs_set[exprs_set$meta_cluster_id == i, colnames(exprs_set) %in% clustering_feature_markers], FUN = mean, MARGIN = 2))
    }
  }
  if (expression_setting == "medians") {
    cluster_matrix <- NULL
    for (i in seq_along(unique(exprs_set$meta_cluster_id))) {
      cluster_matrix <- rbind(cluster_matrix, miscTools::colMedians(exprs_set[exprs_set$meta_cluster_id == i, colnames(exprs_set) %in% clustering_feature_markers]))
    }
  }

  #cluster rows in the Heatmap if "order" is not set
    if (exists("subset_feature_selection")) {
      row_clust_setting <- FALSE
      cluster_matrix <- cluster_matrix[, colnames(cluster_matrix) %in% clustering_feature_markers]
      col_clust_setting <-  TRUE
    } else {
      row_clust_setting <- TRUE
      col_clust_setting <- TRUE
    }

  if (scale == TRUE) {
    #z-normalize feature expression
    #this is done for better contrast in the heatmap
    cluster_matrix <- apply(cluster_matrix, scale, MARGIN = 2)
    cluster_matrix[is.na(cluster_matrix)] <- 0
  }

  rownames(cluster_matrix) <- seq_along(unique(exprs_set$meta_cluster_id))

  cluster_cols <- make_palette("meta_cluster_id")

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
                                  # annotation_legend_param = list(cluster = list(ncol = 2, 
                                  #                                               title = "Cluster",
                                  #                                               title_position = "topcenter",
                                  #                                               at = gtools::mixedsort(names(cluster_cols)),
                                  #                                               grid_height = unit(0.02 * length(feature_markers), "cm"),
                                  #                                               grid_width = unit(0.02 * length(feature_markers), "cm"),
                                  #                                               labels_gp = gpar(fontsize = 0.6 * length(feature_markers)),
                                  #                                               title_gp = gpar(fontsize = 0.6 * length(feature_markers))
                                  #                                               )
                                  #                               ),
                                  col = list(cluster = cluster_cols),
                                  na_col = "white",
                                  show_annotation_name = FALSE,
                                  show_legend = FALSE
                                 )  
  hm <- Heatmap(t(cluster_matrix),
                cluster_rows = row_clust_setting,
                cluster_columns = col_clust_setting,
                row_names_gp = gpar(fontsize = 0.6 * length(feature_markers)),
                column_names_gp = gpar(fontsize = 0.6 * length(feature_markers)), # Text size for row names
                top_annotation = column_ha,
                heatmap_legend_param = list(title = "Scaled expression",
                                            direction = "horizontal",
                                            title_position = "topcenter",
                                            legend_width = unit(0.25 * length(feature_markers), "cm"),
                                            grid_width = unit(0.02 * length(feature_markers), "cm"),
                                            labels_gp = gpar(fontsize = 0.6 * length(feature_markers)),
                                            title_gp = gpar(fontsize = 0.6 * length(feature_markers))
                                          )
                )
  draw(hm,
    heatmap_legend_side = "top",
    row_sub_title_side = "left",
    padding = unit(c(3, 2, 2, 15), "mm"))
  
  invisible(dev.off())

  if (expression_setting == "means" && scale == TRUE) {
    dendrogram_order <<- colnames(t(cluster_matrix))[column_order(hm)]
  }

}


cluster_expr_densities <- function(after_dropping = FALSE, random_sampling = FALSE, target_n = 100000) {
  col_number <- 5
  
  if (after_dropping == TRUE) {
    pdf(file = paste0(output_clustering, "density_cluster_exprs_DROPPED_EVENTS.pdf"), width = 4 * col_number, height = 3 * ceiling(length(clustering_feature_markers) / col_number))
  } else {
    pdf(file = paste0(output_clustering, "density_cluster_exprs.pdf"), width = 4 * col_number, height = 3 * ceiling(length(clustering_feature_markers) / col_number))
  }

  for (i in seq_along(unique(exprs_set$meta_cluster_id))) {
    gg_df <- tidyr::pivot_longer(exprs_set[exprs_set$meta_cluster_id == i, c(clustering_feature_markers)], cols = all_of(clustering_feature_markers), names_to = "antigen") %>%
              mutate(antigen = factor(antigen, levels = c(clustering_feature_markers), ordered = TRUE))
    if (random_sampling == TRUE && nrow(gg_df) > target_n) {
      gg_df <- gg_df[sample(nrow(gg_df), target_n), ]
    }
    print(
      ggplot(gg_df, aes(x = value, y = after_stat(ndensity))) + 
            facet_wrap(~ antigen, scales = "free_x", ncol = col_number,
              labeller = labeller(.cols = label_wrap_gen(width = 20))) +
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
    cols <- group_cols
  }

 if (length(unique(exprs_set[[grouping_var]])) > 40) {
    width_scale <- 40
    text_size <- element_text(size = 10)
  } else {
    width_scale <- 20
    text_size <- element_text(size = 25)
  }
  
  # Set a larger plot size to avoid viewport issues
  pdf(paste0(folder, "PCA_", grouping_var, "_PC_", paste0(dims, collapse = "_"), ".pdf"),
    width = width_scale,  # Increased width
    height = 12  # Increased height
  )

  # Try-catch block to handle potential errors
  tryCatch({
    p <- autoplot(temp_pca, data = averaged_pca["group"], color = "group",
               x = dims[1],
               y = dims[2],
               loadings = TRUE, loadings.colour = 'black',
               loadings.label.colour = 'black', loadings.label = TRUE, loadings.label.size = 10,
               loadings.label.repel = TRUE)
    
    # Add layers individually with error checking
    p <- p + 
          theme_cowplot() +
          scale_color_manual(values = cols, limits = force, labels = scales::label_wrap(25)) +
          labs(color = grouping_var) + 
          theme(text = text_size,
                plot.title = element_text(size=20),
                axis.text.x = element_text(color = "black", size = 20, angle = 0, 
                                           hjust = 0.5, vjust = 0.5, face = "plain"),
                axis.text.y = element_text(color = "black", size = 20, angle = 0, 
                                           hjust = 0.5, vjust = 0.5, face = "plain"),
                axis.title.x.bottom = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                                   size = 25),
                axis.title.y.left = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                                 size = 25)
          )
    
    # Try to add ellipses and points, with fallback options
    tryCatch({
      p <- p + stat_ellipse(type = "norm", level = 0.68, size = 2, alpha = 0.8, 
                           aes(color = group, fill = group))
      p <- p + stat_ellipse(geom = "polygon", alpha = 0.3, aes(fill = group))
    }, error = function(e) {
      message("Could not add ellipses: ", e$message)
    })
    
    # Always add points even if ellipses fail
    p <- p + geom_point(aes(color = group), size = 3.5, alpha = 1)
    
    # Add scale_fill_manual if we have ellipses
    tryCatch({
      p <- p + scale_fill_manual(values = cols, limits = force)
    }, error = function(e) {
      message("Could not add fill scale: ", e$message)
    })
    
    # Print the plot
    print(p)
    
  }, error = function(e) {
    # If full plot fails, create a simpler version without repel labels
    message("Error in plot creation: ", e$message)
    message("Creating simplified plot without repel labels")
    
    p_simple <- autoplot(temp_pca, data = averaged_pca["group"], color = "group",
               x = dims[1],
               y = dims[2],
               loadings = TRUE, loadings.colour = 'black',
               loadings.label = FALSE) +  # No repel labels
        geom_point(aes(color = group), size = 3.5, alpha = 1) +
        theme_cowplot() +
        scale_color_manual(values = cols, limits = force, labels = scales::label_wrap(25)) +
        labs(color = grouping_var, 
             title = paste("PCA simplified (error with full plot)"))
    
    print(p_simple)
  })
  
  invisible(dev.off())
  
  # Return success message
  return(paste("PCA plot for", grouping_var, "completed"))
}


get_contrasting_text_color <- function(hex_color) {
  rgb <- col2rgb(hex_color) / 255
  luminance <- unname(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
  ifelse(luminance > 0.5, "black", "white")
}


umap_plot <- function(grouping_var, module, labels = TRUE, use_scattermore = FALSE) {
  cols <- make_palette(grouping_var)
  text_colors <- sapply(cols, get_contrasting_text_color)

  if (module == "exploration"){
    folder <- output_exploration
  } else if (module == "core"){
    folder <- output_group
    cols <- group_cols
  } else {
    folder <- getwd()
  }



 if (length(unique(exprs_set[[grouping_var]])) > 40) {
    width_scale <- 30
    text_size <- element_text(size = 10)
  } else if (length(unique(exprs_set[[grouping_var]])) > 25) {
    width_scale <- 20
    text_size <- element_text(size = 15)
  } else {
    width_scale <- 15
    text_size <- element_text(size = 25)
  }

  plot_df <- exprs_set[, c("UMAP1", "UMAP2", grouping_var)]

  pdf(paste0(folder, "UMAP_", grouping_var, ".pdf"),
    width = width_scale,
    height = 10
  )

  if (use_scattermore) {
    # Use scattermore for large datasets
    p <- ggplot(plot_df, aes(x = UMAP1, y = UMAP2)) +
          scattermore::geom_scattermore(aes(color = as.factor(.data[[grouping_var]])), alpha = 0.3, pointsize = 3.2, pixels = c(1000, 1000)) + 
          guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3, shape = 19), title = grouping_var)) +
          scale_color_manual(values = cols, labels = scales::label_wrap(25), limits = force) +
          theme_cowplot() +
          theme(text = text_size,
                axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
                axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
                axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
                axis.title.y = element_text(margin = margin(0, 10, 0, 0))
          )
  } else {
    p <- ggplot(plot_df, aes(x = UMAP1, y = UMAP2)) +
        ggrastr::rasterise(geom_point(aes(color = as.factor(.data[[grouping_var]])), alpha = 0.3, size = 1, shape = 19)) +
        # size = 0.5 to restore old version with big points 
        # shape = "." to optimize for execution speed
        guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3, shape = 19), title = grouping_var)) +
        scale_color_manual(values = cols, labels = scales::label_wrap(25), limits = force) +
        theme_cowplot() +
        theme(text = text_size,
              axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
              axis.title.y = element_text(margin = margin(0, 10, 0, 0))
        )
  }

  

  if (labels == TRUE) {
    # Calculate mean coordinates for each group
    mean_coords <- plot_df %>%
      group_by(.data[[grouping_var]]) %>%
      dplyr::summarize(UMAP1 = mean(UMAP1), UMAP2 = mean(UMAP2))
    p <- p + 
          ggnewscale::new_scale_color() +
          geom_label_repel(data = mean_coords, 
            aes(label = str_wrap(.data[[grouping_var]], width = 25),
             fill = as.factor(.data[[grouping_var]]),
             color = as.factor(mean_coords[[grouping_var]])),
            size = 10, max.overlaps = Inf) +
          scale_fill_manual(values = cols, limits = force) +
          scale_color_manual(values = text_colors) +
          guides(fill = "none", color = "none")
  }
  print(p)
  invisible(dev.off())


}

umap_facet <- function(grouping_var, module, column_number = 4, equal_sampling = FALSE, use_scattermore = FALSE, random_sampling = FALSE, target_n = 100000) {
  cols <- make_palette(grouping_var)

  if (module == "exploration") {
    folder <- output_exploration
  } else if (module == "core") {
    folder <- output_group
    cols <- group_cols
  } else {
    folder <- getwd()
  }

  singles_output <- paste0(folder, "UMAP_facet_", grouping_var, "/")
  dir.create(singles_output, showWarnings = FALSE)

  plot_df <- exprs_set[, c("UMAP1", "UMAP2", grouping_var)]

  if (equal_sampling == TRUE) {
    max_equal_sampling <- min(table(as.character(exprs_set[[grouping_var]])))
    plot_df <- plot_df %>% group_by(.data[[grouping_var]]) %>% slice_sample(n = max_equal_sampling)
  } else if (random_sampling == TRUE && target_n < nrow(exprs_set)) {
    plot_df <- plot_df %>% group_by(.data[[grouping_var]]) %>% slice_sample(n = target_n)
  } 
  
  plot_df[[grouping_var]] <- factor(plot_df[[grouping_var]])
  grouping_levels <- levels(plot_df[[grouping_var]])

  p <- list()
  for (s in seq(grouping_levels)) {
    plotted_group <- grouping_levels[s]
    
    if (use_scattermore) {
      p[[s]] <- ggplot(plot_df[plot_df[[grouping_var]] == plotted_group, ], aes(x = UMAP1, y = UMAP2)) +
        scattermore::geom_scattermore(aes(color = '#aeaeae'), alpha = 0.3, pointsize = 3.2, pixels = c(1000, 1000)) + 
        scattermore::geom_scattermore(aes(color = !!sym(grouping_var)), alpha = 0.3, pointsize = 3.2, pixels = c(1000, 1000)) + 
        scale_color_manual(values = cols, limits = force) +
        ggtitle(str_wrap(paste(plotted_group), width = 25)) + 
        theme_cowplot() +
        theme(text = element_text(size = 25),
              axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
              axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
              legend.position = "none"
        )
    } else {
      p[[s]] <- ggplot(plot_df[plot_df[[grouping_var]] == plotted_group, ], aes(x = UMAP1, y = UMAP2)) +
        ggrastr::rasterise(geom_point(color = '#aeaeae', alpha = 0.3, size = 1, shape = 19)) + 
        ggrastr::rasterise(geom_point(aes(color = !!sym(grouping_var)), alpha = 0.3, size = 1, shape = 19, show.legend = F)) + 
        scale_color_manual(values = cols, limits = force) +
        ggtitle(str_wrap(paste(plotted_group), width = 25)) + 
        theme_cowplot() +
        theme(text = element_text(size = 25),
              axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
              axis.title.y = element_text(margin = margin(0, 10, 0, 0))
        )
    }
    
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


umap_expressions <- function(grouping_var = NULL, module, column_number = 4, use_scattermore = FALSE, random_sampling = FALSE, target_n = 100000) {
  
  if (module == "exploration") {
    folder <- output_exploration
  } else if (module == "core") {
    folder <- output_group
  }

  singles_output <- paste0(folder, "UMAP_expressions/")
  dir.create(singles_output, showWarnings = FALSE)


  p <- list()
  for (s in seq(clustering_feature_markers)) {
    plotted_marker <- clustering_feature_markers[s]

    if (!is.null(grouping_var)) {
      plot_df <- exprs_set[, c("UMAP1", "UMAP2", plotted_marker, grouping_var)]
    } else {
      plot_df <- exprs_set[, c("UMAP1", "UMAP2", plotted_marker)]
    }


    if (random_sampling == TRUE && target_n < nrow(exprs_set)) {
      plot_df <- plot_df %>% slice_sample(n = target_n)
    }


    if (use_scattermore) {
       p[[s]] <- ggplot(plot_df %>% arrange(!!sym(plotted_marker)), aes(x = UMAP1, y = UMAP2)) +
        scattermore::geom_scattermore(aes(color = !!sym(plotted_marker)), alpha = 0.3, pointsize = 3.2, pixels = c(1000, 1000)) + 
        scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
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
    } else {
      p[[s]] <- ggplot(plot_df %>% arrange(!!sym(plotted_marker)), aes(x = UMAP1, y = UMAP2)) +
        ggrastr::rasterise(geom_point(aes(color = !!sym(plotted_marker)), alpha = 1, size = 1, shape = 19)) + 
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
    }

    if (!is.null(grouping_var)) {
      plot_df[[grouping_var]] <- factor(plot_df[[grouping_var]])
      grouping_levels <- levels(plot_df[[grouping_var]])

      p[[s]] <- p[[s]] +
                  facet_wrap(~ .data[[grouping_var]], ncol = column_number,
                    labeller = labeller(.cols = label_wrap_gen(width = 25))) +
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


do_corrplot <- function(sample_n = 100000) {
   #prevent from plotting without viewport
   invisible(cormat <- Hmisc::rcorr(as.matrix(exprs_set[sample(nrow(exprs_set), sample_n), clustering_feature_markers]), type = "spearman"))


  #flattenCorrMatrix
  # cormat : matrix of the correlation coefficients
  # pmat : matrix of the correlation p-values
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  = (cormat)[ut],
      p = pmat[ut]
    )
  }
  
  df_cormat <- flattenCorrMatrix(cormat$r, cormat$P)
  
  col <- colorRampPalette(c("blue", "white", "red"))(200)
  
  

  pdf(file = paste0(output_exploration, "channel_correlation_dotplot.pdf"), width = 7, height = 7)
  
  invisible(print(
    corrplot::corrplot(cormat$r, type = "upper", order = "hclust", diag = FALSE, 
                       p.mat = cormat$P, col = col, sig.level = 0.05, insig = "blank"))
  )
  
  invisible(dev.off())
  
}


do_boxplots <- function(data, testing_results = testing_results, grouping_var = group, features, group_by_clusters = TRUE,
                        cluster_var = cluster_var, selected_clusters = NULL, column_number = 4, show_testing = TRUE,
                        show_pvalues = TRUE, show_outliers = TRUE, prefix) {

  if (group_by_clusters == TRUE && length(features) > 1) {
    stop("When grouping by clusters, only one feature can be plotted at a time.")


  } else if (group_by_clusters == FALSE && length(features) > 1) {
      data <- data %>%
                tidyr::pivot_longer(cols = all_of(features), names_to = "feature", values_to = "value")

      if (!is.null(selected_clusters)) {
      data <- data %>% filter(!!sym(cluster_var) %in% selected_clusters)
      }
    if (show_outliers == FALSE) {
      data <- data %>%
        group_by(feature, !!sym(grouping_var)) %>%
        dplyr::filter(
          value >= quantile(value, 0.25) - 1.5 * IQR(value) &
          value <= quantile(value, 0.75) + 1.5 * IQR(value)
        ) %>%
        ungroup()
    }

    p <- ggplot(data, aes(x = !!sym(grouping_var), y = value, color = !!sym(grouping_var))) +
      geom_boxplot(size = 2, outlier.shape = NA) +
      geom_jitter(size = 5, width = 0.1, alpha = 1) +
      xlab(grouping_var) +
      ylab('Marker Expression') +
      scale_color_manual(values = group_cols, limits = force) +
      facet_wrap(~ feature, scales = "free", ncol = column_number,
        labeller = labeller(.cols = label_wrap_gen(width = 25))) +
      theme_cowplot() +
      theme(
        text = element_text(size = 45),
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 30, angle = 45, hjust = 1, vjust = 1, face = "plain"),
        axis.text.y = element_text(color = "black", size = 45, angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
        plot.margin = margin(t = 20, r = 0, b = 0, l = 10),
        strip.background = element_rect(fill = "#ffffff")
      )

    if (show_testing == TRUE) {
      testing_results$feature <- testing_results$.y.
      if (show_pvalues == TRUE) {
        y_positions <- c()
        for (i in unique(testing_results[["feature"]])) {
          temp <- get_y_position(data[data$feature == i, ], reformulate(grouping_var, "value"), step.increase = 0.12, scales = "free")$y.position
          y_positions <- c(y_positions, temp)
        }
        testing_results$y.position <- y_positions
        testing_results$p.adj <- round(testing_results$p.adj, 3)
        p <- p + stat_pvalue_manual(testing_results, label = "p.adj", hide.ns = FALSE, size = 8, tip.length = 0.01)
      } else {
        y_positions <- c()
        for (i in unique(testing_results[["feature"]])) {
          temp <- get_y_position(data[data$feature == i, ], reformulate(grouping_var, "value"), step.increase = 0.12, scales = "free")$y.position
          y_positions <- c(y_positions, temp)
        }
        testing_results$y.position <- y_positions

        nrow_signif <- testing_results %>%
                      dplyr::group_by(.data[["feature"]]) %>%
                      dplyr::summarise(n_signif = sum(p.adj.signif != "ns"))
        
        if (sum(nrow_signif$n_signif > 0) > 0) {
          filtered_testing <- c()
          for (i in unique(testing_results[["feature"]])) {
           marker_nrow_signif <- nrow_signif$n_signif[nrow_signif[["feature"]] == i]
           if (marker_nrow_signif > 0) {
            temp_y_positions <- testing_results$y.position[testing_results[["feature"]] == i]
            temp_testing <- testing_results %>% dplyr::filter(.data[["feature"]] == i & p.adj.signif != "ns")
            temp_testing$y.position <- temp_y_positions[1:marker_nrow_signif]
            filtered_testing <- rbind(filtered_testing, temp_testing)
           }
          }
          testing_results <- filtered_testing
          p <- p + stat_pvalue_manual(testing_results, label = "p.adj.signif", hide.ns = FALSE, size = 15, tip.length = 0.01)
        }
      }
    }

    n_features <- length(features)
    pdf(paste0(output_group, prefix, "_", "boxplot_grid", ".pdf"),
      width = 8 * column_number,
      height = 12 * ceiling(n_features / column_number)
    )
    print(p)
    invisible(dev.off())

    singles_output <- paste0(output_group, prefix, "/")
    dir.create(singles_output)

    for (facet in unique(testing_results[["feature"]])) {
      data_subset <- data[data$feature == facet, ]

      p_subset <- ggplot(data_subset, aes(x = !!sym(grouping_var), y = value, color = !!sym(grouping_var))) +
        geom_boxplot(size = 2, outlier.shape = NA) +
        geom_jitter(size = 5, width = 0.1, alpha = 1) +
        ggtitle(paste(facet)) +
        xlab(grouping_var) +
        ylab('Marker Expression') +
        scale_color_manual(values = group_cols, limits = force) +
        theme_cowplot() +
        theme(
          text = element_text(size = 45),
          legend.position = "none",
          axis.text.x = element_text(color = "black", size = 30, angle = 45, hjust = 1, vjust = 1, face = "plain"),
          axis.text.y = element_text(color = "black", size = 45, angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
          plot.margin = margin(t = 20, r = 0, b = 0, l = 10),
          strip.background = element_rect(fill = "#ffffff")
        )

      if (show_testing == TRUE) {
        if (show_pvalues == TRUE) {
          p_subset <- p_subset + stat_pvalue_manual(testing_results[testing_results[["feature"]] == facet, ], label = "p.adj", hide.ns = FALSE, size = 8, tip.length = 0.01)
        } else {
          if (sum(nrow_signif$n_signif > 0) > 0) {
            p_subset <- p_subset + stat_pvalue_manual(filtered_testing[filtered_testing[["feature"]] == facet, ], label = "p.adj.signif", hide.ns = FALSE, size = 8, tip.length = 0.01)
          }
        }
      }

      pdf(paste0(singles_output, prefix, "_", "boxplot_", facet, ".pdf"),
        width = 8,
        height = 12
      )
      print(p_subset)
      invisible(dev.off())
    }


  } else if (group_by_clusters == TRUE && length(features) == 1) {
    data <- data %>%
                tidyr::pivot_longer(cols = all_of(features), names_to = "feature", values_to = "value")
    if (!is.null(selected_clusters)) {
      data <- data %>% dplyr::filter(!!sym(cluster_var) %in% selected_clusters)
    }
    if (show_outliers == FALSE) {
      data <- data %>%
        group_by(!!sym(cluster_var), !!sym(grouping_var)) %>%
        dplyr::filter(
          value >= quantile(value, 0.25) - 1.5 * IQR(value) &
          value <= quantile(value, 0.75) + 1.5 * IQR(value)
        ) %>%
        ungroup()
    }

    p <- ggplot(data, aes(x = !!sym(grouping_var), y = value, color = !!sym(grouping_var))) +
      geom_boxplot(size = 2, outlier.shape = NA) +
      geom_jitter(size = 5, width = 0.1, alpha = 1) +
      xlab(grouping_var) +
      ylab('Cluster Abundance [%]') +
      scale_color_manual(values = group_cols, limits = force) +
      facet_wrap(~ .data[[cluster_var]], scales = "free", ncol = column_number,
        labeller = labeller(.cols = label_wrap_gen(width = 25))) +
      theme_cowplot() +
      theme(
        text = element_text(size = 45),
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 30, angle = 45, hjust = 1, vjust = 1, face = "plain"),
        axis.text.y = element_text(color = "black", size = 45, angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
        plot.margin = margin(t = 20, r = 0, b = 0, l = 10),
        strip.background = element_rect(fill = "#ffffff")
      )

    if (show_testing == TRUE) {
      if (show_pvalues == TRUE) {
        y_positions <- c()
        for (i in unique(testing_results[[cluster_var]])) {
          temp <- get_y_position(data[data[[cluster_var]] == i, ], reformulate(grouping_var, "value"), step.increase = 0.12, scales = "free")$y.position
          y_positions <- c(y_positions, temp)
        }

        if (length(y_positions) > nrow(testing_results)) {
          # extra y positions, this means that some cluster-group combinations
          # had variance of 0 and were removed to avoid errors in non-parametric tests
          # skip plotting the boxplots
          return("Not plotting boxplots due to variance of 0 in some cluster-group combinations.
                  This would break the p value rendering.")
        }

        testing_results$y.position <- y_positions
        testing_results$p.adj <- round(testing_results$p.adj, 3)
        p <- p + stat_pvalue_manual(testing_results, label = "p.adj", hide.ns = FALSE, size = 8, tip.length = 0.01)

      } else {
        y_positions <- c()
        for (i in unique(testing_results[[cluster_var]])) {
          temp <- get_y_position(data[data[[cluster_var]] == i, ], reformulate(grouping_var, "value"), step.increase = 0.12, scales = "free")$y.position
          y_positions <- c(y_positions, temp)
        }

        if (length(y_positions) > nrow(testing_results)) {
          # extra y positions, this means that some cluster-group combinations
          # had variance of 0 and were removed to avoid errors in non-parametric tests
          # skip plotting the boxplots
          return("Not plotting boxplots due to variance of 0 in some cluster-group combinations.
                  This would break the p value rendering.")
        }



        testing_results$y.position <- y_positions

        nrow_signif <- testing_results %>%
                      dplyr::group_by(.data[[cluster_var]]) %>%
                      dplyr::summarise(n_signif = sum(p.adj.signif != "ns"))
        
        if (sum(nrow_signif$n_signif > 0) > 0) {
          filtered_testing <- c()
          for (i in unique(testing_results[[cluster_var]])) {
           cluster_nrow_signif <- nrow_signif$n_signif[nrow_signif[[cluster_var]] == i]
           if (cluster_nrow_signif > 0) {
            temp_y_positions <- testing_results$y.position[testing_results[[cluster_var]] == i]
            temp_testing <- testing_results %>% dplyr::filter(.data[[cluster_var]] == i & p.adj.signif != "ns")
            temp_testing$y.position <- temp_y_positions[1:cluster_nrow_signif]
            filtered_testing <- rbind(filtered_testing, temp_testing)
           }
          }
          testing_results <- filtered_testing
          p <- p + stat_pvalue_manual(testing_results, label = "p.adj.signif", hide.ns = FALSE, size = 15, tip.length = 0.01)
        }
      }
    }
    n_clusters <- length(unique(data[[cluster_var]]))
    pdf(paste0(output_group, prefix, "_", "boxplot_grid", ".pdf"),
      width = 8 * column_number,
      height = 12 * ceiling(n_clusters / column_number)
    )
    print(p)
    invisible(dev.off())

    singles_output <- paste0(output_group, prefix, "/")
    dir.create(singles_output)

    for (facet in unique(data[[cluster_var]])) {
      data_subset <- data[data[[cluster_var]] == facet, ]

      p_subset <- ggplot(data_subset, aes(x = !!sym(grouping_var), y = value, color = !!sym(grouping_var))) +
        geom_boxplot(size = 2, outlier.shape = NA) +
        geom_jitter(size = 5, width = 0.1, alpha = 1) +
        ggtitle(paste(facet)) +
        xlab(grouping_var) +
        ylab('Cluster Abundance [%]') +
        scale_color_manual(values = group_cols, limits = force) +
        theme_cowplot() +
        theme(
          text = element_text(size = 45),
          legend.position = "none",
          axis.text.x = element_text(color = "black", size = 30, angle = 45, hjust = 1, vjust = 1, face = "plain"),
          axis.text.y = element_text(color = "black", size = 45, angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
          plot.margin = margin(t = 20, r = 0, b = 0, l = 10),
          strip.background = element_rect(fill = "#ffffff")
        )

      if (show_testing == TRUE) {
        if (show_pvalues == TRUE) {
          p_subset <- p_subset + stat_pvalue_manual(testing_results[testing_results[[cluster_var]] == facet, ], label = "p.adj", hide.ns = FALSE, size = 8, tip.length = 0.01)
        } else {
          if (sum(nrow_signif$n_signif > 0) > 0) {
            p_subset <- p_subset + stat_pvalue_manual(filtered_testing[filtered_testing[[cluster_var]] == facet, ], label = "p.adj.signif", hide.ns = FALSE, size = 8, tip.length = 0.01)
          }
        }
      }
      
      pdf(paste0(singles_output, prefix, "_", "boxplot_", facet, ".pdf"),
        width = 8,
        height = 12
      )
      print(p_subset)
      invisible(dev.off())
    }

  

  }
  
}


cluster_abundance_heatmaps <- function(data = data, grouping_var = group, features = features, cluster_var = cluster_var, selected_clusters = selected_clusters, cluster_ordering = FALSE, prefix = prefix) {

  n_clusters <- length(unique(data[[cluster_var]]))
  # per patient version
  abu_heat_mat <- data

  if (!is.null(selected_clusters)) {
    abu_heat_mat <- abu_heat_mat %>% filter(!!sym(cluster_var) %in% selected_clusters)
  }

  abu_heat_mat <- abu_heat_mat %>%
    select(-count) %>%
    tidyr::pivot_wider(names_from = !!sym(cluster_var), values_from = prop)

  if (cluster_ordering == TRUE && exists("dendrogram_order")) {
      # Ensure dendrogram_order contains valid column names
      dendrogram_order <- as.character(dendrogram_order)
      
      # Add missing columns with 0s
      missing_cols <- setdiff(dendrogram_order, colnames(abu_heat_mat))
      if (length(missing_cols) > 0) {
          for (col in missing_cols) {
              abu_heat_mat[, col] <- 0  # Fixed: single brackets
          }
      }
      
      # Safely reorder columns
      available_ordered_cols <- intersect(dendrogram_order, colnames(abu_heat_mat))
      rest_of_columns <- abu_heat_mat[, !colnames(abu_heat_mat) %in% dendrogram_order, drop = FALSE]
      abu_heat_mat <- cbind(abu_heat_mat[, available_ordered_cols, drop = FALSE], rest_of_columns)
  }

  if (cluster_ordering == TRUE && exists("dendrogram_order")) {
    clust_cols <- FALSE
  } else {
    clust_cols <- TRUE
  }

  row_annot <- as.factor(abu_heat_mat[[grouping_var]])
  id_annot <- abu_heat_mat[["id"]]

  abu_heat_mat <- abu_heat_mat %>% select(-c(id, !!sym(grouping_var)))
  abu_heat_mat <- as.matrix(abu_heat_mat)
  rownames(abu_heat_mat) <- id_annot

  cols <- make_palette(cluster_var)

  column_ha <- HeatmapAnnotation(cluster = colnames(abu_heat_mat),
                                annotation_legend_param = list(
                                  cluster = list(
                                    ncol = 2,
                                    title = "Cluster",
                                    title_position = "topcenter",
                                    at = names(cols),
                                    grid_height = unit(0.02 * (nrow(abu_heat_mat) + 2), "cm"),  
                                    grid_width = unit(0.02 * (nrow(abu_heat_mat) + 2), 'cm'),
                                    labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5)),
                                    title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5))
                                  )
                                ),
                                col = list(cluster = cols),
                                na_col = "white",
                                show_annotation_name = FALSE,
                                show_legend = FALSE
  )


  row_ha <- rowAnnotation(group = row_annot,
                          annotation_legend_param = list(
                            group = list(
                              ncol = 2,
                              title = "Group",
                              title_position = "topcenter",
                              at = names(group_cols),
                              grid_height = unit(0.02 * length(row_annot), "cm"),
                              grid_width = unit(0.02 * length(row_annot), 'cm'),
                              labels_gp = gpar(fontsize = 0.6 * length(row_annot)),
                              title_gp = gpar(fontsize = 0.6 * length(row_annot))
                            )
                          ),
                          col = list(group = group_cols),
                          na_col = "white",
                          show_annotation_name = FALSE
  )


  hm <- Heatmap(abu_heat_mat,
                cluster_rows = TRUE,
                cluster_columns = clust_cols,
                row_names_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5)),
                column_names_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5)),
                top_annotation = column_ha,
                right_annotation = row_ha,
                heatmap_legend_param = list(
                  title = "Cluster abundance [%]",
                  direction = 'horizontal',
                  title_position = "topcenter",
                  legend_width = unit(0.25 * (nrow(abu_heat_mat) + 2), "cm"),
                  grid_width = unit(0.25 * (nrow(abu_heat_mat) + 2), 'cm'),
                  labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5)),
                  title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5))
                ))

  pdf(file = paste0(output_group, prefix, "_heatmap_per_sample.pdf"), width = 1.1 * ncol(abu_heat_mat), height = 2 + 0.7 * nrow(abu_heat_mat))
  draw(hm,
      heatmap_legend_side = 'top',
      row_sub_title_side = 'left',
      padding = unit(c(40, 0, 0, 40), "pt"))

  invisible(dev.off())


  #scaled per patient version
  abu_heat_mat <- data

  if (!is.null(selected_clusters)) {
    data <- data %>% filter(!!sym(cluster_var) %in% selected_clusters)
  }

  abu_heat_mat <- abu_heat_mat %>%
    select(-count) %>%
    tidyr::pivot_wider(names_from = !!sym(cluster_var), values_from = prop)

  row_annot <- as.factor(abu_heat_mat[[grouping_var]])
  id_annot <- abu_heat_mat[["id"]]

  abu_heat_mat <- abu_heat_mat %>% select(-c(id, !!sym(grouping_var)))
  abu_heat_mat <- as.matrix(abu_heat_mat)
  abu_heat_mat <- apply(abu_heat_mat, MARGIN = 2, FUN = scale)
  rownames(abu_heat_mat) <- id_annot

  cols <- make_palette(cluster_var)

  column_ha <- HeatmapAnnotation(cluster = colnames(abu_heat_mat),
                                annotation_legend_param = list(
                                  cluster = list(
                                    ncol = 2,
                                    title = "Cluster",
                                    title_position = "topcenter",
                                    at = names(cols),
                                    grid_height = unit(0.02 * (nrow(abu_heat_mat) + 2), "cm"),
                                    grid_width = unit(0.02 * (nrow(abu_heat_mat) + 2), 'cm'),
                                    labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5)),
                                    title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5))
                                  )
                                ),
                                col = list(cluster = cols),
                                na_col = "white",
                                show_annotation_name = FALSE,
                                show_legend = FALSE
  )

  row_ha <- rowAnnotation(group = row_annot,
                          annotation_legend_param = list(
                            group = list(
                              ncol = 2,
                              title = "Group",
                              title_position = "topcenter",
                              at = names(group_cols),
                              grid_height = unit(0.02 * (nrow(abu_heat_mat) + 2), "cm"),
                              grid_width = unit(0.02 * (nrow(abu_heat_mat) + 2), 'cm'),
                              labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5)),
                              title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5))
                            )
                          ),
                          col = list(group = group_cols),
                          na_col = "white",
                          show_annotation_name = FALSE
  )


  hm <- Heatmap(abu_heat_mat,
                cluster_rows = TRUE,
                cluster_columns = clust_cols,
                row_names_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5)),
                column_names_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5)),
                top_annotation = column_ha,
                right_annotation = row_ha,
                heatmap_legend_param = list(
                  title = "Scaled cluster abundance",
                  direction = 'horizontal',
                  title_position = "topcenter",
                  legend_width = unit(0.25 * (nrow(abu_heat_mat) + 2), "cm"),
                  grid_width = unit(0.25 * (nrow(abu_heat_mat) + 2), 'cm'),
                  labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5)),
                  title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 5))
                ))

  pdf(file = paste0(output_group, prefix, "_heatmap_per_sample_scaled.pdf"), width = 1.1 * ncol(abu_heat_mat), height = 2 + 0.7 * nrow(abu_heat_mat))
  draw(hm,
      heatmap_legend_side = 'top',
      row_sub_title_side = 'left',
      padding = unit(c(40, 0, 0, 40), "pt"))

  invisible(dev.off())




  # per group version
  abu_heat_mat <- data

  if (!is.null(selected_clusters)) {
    abu_heat_mat <- abu_heat_mat %>% filter(!!sym(cluster_var) %in% selected_clusters)
  }

  abu_heat_mat <- abu_heat_mat %>%
    select(-c(count)) %>%
    tidyr::pivot_wider(names_from = !!sym(cluster_var), values_from = prop)

  abu_heat_mat <- abu_heat_mat %>% select(-c(id))

  abu_heat_mat <- abu_heat_mat %>% dplyr::group_by(!!sym(group)) %>%
                    dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE))

  if (cluster_ordering == TRUE && exists("dendrogram_order")) {
      # Ensure dendrogram_order contains valid column names
      dendrogram_order <- as.character(dendrogram_order)
      
      # Add missing columns with 0s
      missing_cols <- setdiff(dendrogram_order, colnames(abu_heat_mat))
      if (length(missing_cols) > 0) {
          for (col in missing_cols) {
              abu_heat_mat[, col] <- 0  # Fixed: single brackets
          }
      }
      
      # Safely reorder columns
      available_ordered_cols <- intersect(dendrogram_order, colnames(abu_heat_mat))
      rest_of_columns <- abu_heat_mat[, !colnames(abu_heat_mat) %in% dendrogram_order, drop = FALSE]
      abu_heat_mat <- cbind(abu_heat_mat[, available_ordered_cols, drop = FALSE], rest_of_columns)
  }

  row_annot <- as.factor(abu_heat_mat[[grouping_var]])


  abu_heat_mat <- abu_heat_mat %>% select(-c(!!sym(grouping_var)))
  abu_heat_mat <- as.matrix(abu_heat_mat)
  rownames(abu_heat_mat) <- row_annot

  cols <- make_palette(cluster_var)


  column_ha <- HeatmapAnnotation(cluster = colnames(abu_heat_mat),
                                annotation_legend_param = list(
                                  cluster = list(
                                    ncol = 2,
                                    title = "Cluster",
                                    title_position = "topcenter",
                                    at = names(cols),
                                    grid_height = unit(0.02 * (nrow(abu_heat_mat) + 2), "cm"),
                                    grid_width = unit(0.02 * (nrow(abu_heat_mat) + 2), 'cm'),
                                    labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                                    title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10))
                                  )
                                ),
                                col = list(cluster = cols),
                                na_col = "white",
                                show_annotation_name = FALSE,
                                show_legend = FALSE
  )

  row_ha <- rowAnnotation(group = row_annot,
                          annotation_legend_param = list(
                            group = list(
                              ncol = 2,
                              title = "Group",
                              title_position = "topcenter",
                              at = names(group_cols),
                              grid_height = unit(0.02 * (nrow(abu_heat_mat) + 2), "cm"),
                              grid_width = unit(0.02 * (nrow(abu_heat_mat) + 2), 'cm'),
                              labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                              title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10))
                            )
                          ),
                          col = list(group = group_cols),
                          na_col = "white",
                          show_annotation_name = FALSE
  )


  hm <- Heatmap(abu_heat_mat,
                cluster_rows = TRUE,
                cluster_columns = clust_cols,
                row_names_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                column_names_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                top_annotation = column_ha,
                right_annotation = row_ha,
                heatmap_legend_param = list(
                  title = "Cluster abundance [%]",
                  direction = 'horizontal',
                  title_position = "topcenter",
                  legend_width = unit(0.25 * (nrow(abu_heat_mat) + 2), "cm"),
                  grid_width = unit(0.25 * (nrow(abu_heat_mat) + 2), 'cm'),
                  labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                  title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10))
                ))

  pdf(file = paste0(output_group, prefix, "_heatmap_per_group.pdf"), width = 1.1 * ncol(abu_heat_mat), height = 3 + 0.5 * nrow(abu_heat_mat))
  draw(hm,
      heatmap_legend_side = 'top',
      row_sub_title_side = 'left',
      padding = unit(c(40, 0, 0, 40), "pt"))

  invisible(dev.off())


  #scaled per patient version
  abu_heat_mat <- data

  if (!is.null(selected_clusters)) {
    abu_heat_mat <- abu_heat_mat %>% filter(!!sym(cluster_var) %in% selected_clusters)
  }

  abu_heat_mat <- abu_heat_mat %>%
    select(-c(count)) %>%
    tidyr::pivot_wider(names_from = !!sym(cluster_var), values_from = prop)

  abu_heat_mat <- abu_heat_mat %>% select(-c(id))

  abu_heat_mat <- abu_heat_mat %>% dplyr::group_by(!!sym(group)) %>%
                    dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE))

  if (cluster_ordering == TRUE && exists("dendrogram_order")) {
      # Ensure dendrogram_order contains valid column names
      dendrogram_order <- as.character(dendrogram_order)
      
      # Add missing columns with 0s
      missing_cols <- setdiff(dendrogram_order, colnames(abu_heat_mat))
      if (length(missing_cols) > 0) {
          for (col in missing_cols) {
              abu_heat_mat[, col] <- 0  # Fixed: single brackets
          }
      }
      
      # Safely reorder columns
      available_ordered_cols <- intersect(dendrogram_order, colnames(abu_heat_mat))
      rest_of_columns <- abu_heat_mat[, !colnames(abu_heat_mat) %in% dendrogram_order, drop = FALSE]
      abu_heat_mat <- cbind(abu_heat_mat[, available_ordered_cols, drop = FALSE], rest_of_columns)
  }

  row_annot <- as.factor(abu_heat_mat[[grouping_var]])


  abu_heat_mat <- abu_heat_mat %>% select(-c(!!sym(grouping_var)))
  abu_heat_mat <- as.matrix(abu_heat_mat)
  abu_heat_mat <- abu_heat_mat[, apply(abu_heat_mat, MARGIN = 2, FUN = is.numeric)]
  abu_heat_mat <- apply(abu_heat_mat, MARGIN = 2, FUN = scale)
  rownames(abu_heat_mat) <- row_annot

  cols <- make_palette(cluster_var)

  column_ha <- HeatmapAnnotation(cluster = colnames(abu_heat_mat),
                                annotation_legend_param = list(
                                  cluster = list(
                                    ncol = 2,
                                    title = "Cluster",
                                    title_position = "topcenter",
                                    at = names(cols),
                                    grid_height = unit(0.02 * (nrow(abu_heat_mat) + 2), "cm"),
                                    grid_width = unit(0.02 * (nrow(abu_heat_mat) + 2), 'cm'),
                                    labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                                    title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10))
                                  )
                                ),
                                col = list(cluster = cols),
                                na_col = "white",
                                show_annotation_name = FALSE,
                                show_legend = FALSE
  )

  row_ha <- rowAnnotation(group = row_annot,
                          annotation_legend_param = list(
                            group = list(
                              ncol = 2,
                              title = "Group",
                              title_position = "topcenter",
                              at = names(group_cols),
                              grid_height = unit(0.02 * (nrow(abu_heat_mat) + 2), "cm"),
                              grid_width = unit(0.02 * (nrow(abu_heat_mat) + 2), 'cm'),
                              labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                              title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10))
                            )
                          ),
                          col = list(group = group_cols),
                          na_col = "white",
                          show_annotation_name = FALSE
  )

  hm <- Heatmap(abu_heat_mat,
                cluster_rows = TRUE,
                cluster_columns = clust_cols,
                row_names_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                column_names_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                top_annotation = column_ha,
                right_annotation = row_ha,
                heatmap_legend_param = list(
                  title = "Scaled cluster abundance",
                  direction = 'horizontal',
                  title_position = "topcenter",
                  legend_width = unit(0.25 * (nrow(abu_heat_mat) + 2), "cm"),
                  grid_width = unit(0.25 * (nrow(abu_heat_mat) + 2), 'cm'),
                  labels_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10)),
                  title_gp = gpar(fontsize = 0.6 * (nrow(abu_heat_mat) + 10))
                ))

  pdf(file = paste0(output_group, prefix, "_heatmap_per_group_scaled.pdf"), width = 1.1 * ncol(abu_heat_mat), height = 3 + 0.5 * nrow(abu_heat_mat))
  draw(hm,
      heatmap_legend_side = 'top',
      row_sub_title_side = 'left',
      padding = unit(c(40, 0, 0, 40), "pt"))

  invisible(dev.off())

}


cluster_testing_heatmaps <- function(data = data, testing_results = testing_results, grouping_var = group, features = features,
                                cluster_var = cluster_var, selected_clusters = selected_clusters, cluster_ordering = FALSE, prefix = prefix) {

  if (nrow(testing_results) > 0) {
    test_mat <- testing_results
    clusters_present <- as.character(unique(test_mat[[cluster_var]]))
    test_mat_temp <- test_mat %>% tidyr::pivot_wider(id_cols = c(group1, group2), names_from = c(!!sym(cluster_var)), values_from = "p.adj")
    test_mat <- test_mat_temp
    test_mat_temp <- test_mat_temp %>% select(all_of(clusters_present)) %>% as.matrix()
    test_mat_temp[is.na(test_mat_temp)] <- 1

    rownames(test_mat_temp) <- tidyr::unite(test_mat, pair, c(group1, group2), remove = FALSE, sep = ' / ') %>% pull(pair)

    #function to cbind to an empty dataframe
    cbind.fill <- function(...) {
      nm <- list(...) 
      nm <- lapply(nm, as.matrix)
      n <- max(sapply(nm, nrow)) 
      do.call(cbind, lapply(nm, function (x) 
        rbind(x, matrix(, n - nrow(x), ncol(x))))) 
    }

    
    colz <- circlize::colorRamp2(breaks = c(0, 0.05), 
                                 colors = c('#000000', "white"))

    # Add missing columns filled with value 1
    unique_clusters <- unique(data[[cluster_var]])
    missing_clusters <- setdiff(unique_clusters, clusters_present)
    
    if (length(missing_clusters) > 0) {
      test_mat_temp <- cbind(test_mat_temp, matrix(1, nrow = nrow(test_mat_temp), ncol = length(missing_clusters)))
      colnames(test_mat_temp)[(ncol(test_mat_temp) - length(missing_clusters) + 1):ncol(test_mat_temp)] <- missing_clusters
    }

    if (cluster_ordering == TRUE && exists("dendrogram_order")) {
      if (nrow(test_mat_temp) > 1) {
        # Ensure dendrogram_order contains valid column names
        dendrogram_order <- as.character(dendrogram_order)
        
        # Add missing columns with 0s
        missing_cols <- setdiff(dendrogram_order, colnames(test_mat_temp))
        if (length(missing_cols) > 0) {
            for (col in missing_cols) {
                test_mat_temp[, col] <- 0  # Fixed: single brackets
            }
        }
        
        # Safely reorder columns
        available_ordered_cols <- intersect(dendrogram_order, colnames(test_mat_temp))
        rest_of_columns <- test_mat_temp[, !colnames(test_mat_temp) %in% dendrogram_order, drop = FALSE]
        test_mat_temp <- cbind(test_mat_temp[, available_ordered_cols, drop = FALSE], rest_of_columns)
      }
    }


    pdf(file = paste0(output_group, prefix, "_testing_heatmap.pdf"), width = 1.1 * ncol(test_mat_temp), height = 3 + 0.5 * nrow(test_mat_temp))

      
      pairname_font <- 20

      row_ha = rowAnnotation(group1 = test_mat$group1,
                            group2 = test_mat$group2,
                            annotation_legend_param = list(
                              group1 = list(
                                ncol = 1, 
                                title = "Group 1",
                                title_position = "topcenter",
                                at = unique(test_mat$group1),
                                grid_height = unit(0.2, "cm"),
                                grid_width = unit(0.2, 'cm'),
                                labels_gp = gpar(fontsize = 0.6 * (nrow(test_mat_temp) + 10)),
                                title_gp = gpar(fontsize = 0.6 * (nrow(test_mat_temp) + 10))
                              ),
                              group2 = list(
                                ncol = 1, 
                                title = "Group 2",
                                title_position = "topcenter",
                                at = unique(test_mat$group2),
                                grid_height = unit(0.2, "cm"),
                                grid_width = unit(0.2, 'cm'),
                                labels_gp = gpar(fontsize = 0.6 * (nrow(test_mat_temp) + 10)),
                                title_gp = gpar(fontsize = 0.6 * (nrow(test_mat_temp) + 10))
                              )
                            ),
                            col = list(group1 = group_cols, group2 = group_cols),
                            na_col = "white",
                            show_annotation_name = FALSE,
                            show_legend = FALSE
      )

      pvhmap <- Heatmap(test_mat_temp, col = colz, right_annotation = row_ha,
                        column_names_gp = gpar(fontsize =  0.8 * (nrow(test_mat_temp) + 10)),
                        cluster_rows = FALSE, cluster_columns = FALSE,
                        heatmap_legend_param = list(at = c(0, 0.05),
                                                    title = "Adjusted p values",
                                                    direction = 'horizontal',
                                                    title_position = "topcenter",
                                                    legend_width = unit(0.25 * (nrow(test_mat_temp) + 2), "cm"),
                                                    grid_width = unit(0.25 * (nrow(test_mat_temp) + 2), 'cm'),
                                                    labels_gp = gpar(fontsize = 0.6 * (nrow(test_mat_temp) + 10)),
                                                    title_gp = gpar(fontsize = 0.6 * (nrow(test_mat_temp) + 10))
                                                    )
                      
      )

      draw(pvhmap,
          heatmap_legend_side = 'top',
          padding = unit(c(20, 0, 0, 40), "pt"), # Move annotation legend to the left side)
          annotation_legend_side = "right",
            legend_grouping = "original")   #padding bot, left, top, right
      invisible(dev.off())
  }
}



marker_average_heatmaps <- function(data = data,
grouping_var = group,
features = features,
prefix = prefix, 
pairing_var = NULL,
custom_marker_order = NULL) {

  # per patient version
  expr_heat_mat <- data

  row_annot <- as.factor(expr_heat_mat[[grouping_var]])
  id_annot <- expr_heat_mat[["id"]]

  if (!is.null(pairing_var)) {
    expr_heat_mat <- expr_heat_mat %>% dplyr::select(-c(!!sym(pairing_var)))
  }
  expr_heat_mat <- expr_heat_mat %>% select(-c(id, !!sym(grouping_var)))
  expr_heat_mat <- as.matrix(expr_heat_mat)
  rownames(expr_heat_mat) <- id_annot


  row_ha <- rowAnnotation(group = row_annot,
                          annotation_legend_param = list(
                            group = list(
                              ncol = 2,
                              title = "Group",
                              title_position = "topcenter",
                              at = names(group_cols),
                              grid_height = unit(0.02 * length(row_annot), "cm"),
                              grid_width = unit(0.02 * length(row_annot), 'cm'),
                              labels_gp = gpar(fontsize = 0.6 * length(row_annot)),
                              title_gp = gpar(fontsize = 0.6 * length(row_annot))
                            )
                          ),
                          col = list(group = group_cols),
                          na_col = "white",
                          show_annotation_name = FALSE
  )


  hm <- Heatmap(expr_heat_mat,
                cluster_rows = TRUE,
                cluster_columns = TRUE,
                row_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                column_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                right_annotation = row_ha,
                heatmap_legend_param = list(
                  title = "Mean Marker Expression",
                  direction = 'horizontal',
                  title_position = "topcenter",
                  legend_width = unit(0.25 * (nrow(expr_heat_mat) + 2), "cm"),
                  grid_width = unit(0.25 * (nrow(expr_heat_mat) + 2), 'cm'),
                  labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                  title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                ))

  pdf(file = paste0(output_group, prefix, "_heatmap_per_sample.pdf"), width = 1.1 * ncol(expr_heat_mat), height = 2 + 0.7 * nrow(expr_heat_mat))
  draw(hm,
      heatmap_legend_side = 'top',
      row_sub_title_side = 'left',
      padding = unit(c(40, 0, 0, 40), "pt"))

  invisible(dev.off())


  #scaled per patient version
  expr_heat_mat <- data

  row_annot <- as.factor(expr_heat_mat[[grouping_var]])
  id_annot <- expr_heat_mat[["id"]]


  if (!is.null(pairing_var)) {
    expr_heat_mat <- expr_heat_mat %>% dplyr::select(-c(!!sym(pairing_var)))
  }
  expr_heat_mat <- expr_heat_mat %>% select(-c(id, !!sym(grouping_var)))
  expr_heat_mat <- as.matrix(expr_heat_mat)
  expr_heat_mat <- expr_heat_mat[, apply(expr_heat_mat, MARGIN = 2, FUN = is.numeric)]
  expr_heat_mat <- apply(expr_heat_mat, MARGIN = 2, FUN = scale)
  rownames(expr_heat_mat) <- id_annot


  row_ha <- rowAnnotation(group = row_annot,
                          annotation_legend_param = list(
                            group = list(
                              ncol = 2,
                              title = "Group",
                              title_position = "topcenter",
                              at = names(group_cols),
                              grid_height = unit(0.02 * (nrow(expr_heat_mat) + 2), "cm"),
                              grid_width = unit(0.02 * (nrow(expr_heat_mat) + 2), 'cm'),
                              labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                              title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                            )
                          ),
                          col = list(group = group_cols),
                          na_col = "white",
                          show_annotation_name = FALSE
  )


  hm <- Heatmap(expr_heat_mat,
                cluster_rows = TRUE,
                cluster_columns = TRUE,
                row_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                column_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                right_annotation = row_ha,
                heatmap_legend_param = list(
                  title = "Scaled cluster abundance",
                  direction = 'horizontal',
                  title_position = "topcenter",
                  legend_width = unit(0.25 * (nrow(expr_heat_mat) + 2), "cm"),
                  grid_width = unit(0.25 * (nrow(expr_heat_mat) + 2), 'cm'),
                  labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                  title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                ))

  pdf(file = paste0(output_group, prefix, "_heatmap_per_sample_scaled.pdf"), width = 1.1 * ncol(expr_heat_mat), height = 2 + 0.7 * nrow(expr_heat_mat))
  draw(hm,
      heatmap_legend_side = 'top',
      row_sub_title_side = 'left',
      padding = unit(c(40, 0, 0, 40), "pt"))

  invisible(dev.off())




  # per group version
  expr_heat_mat <- data

  if (!is.null(pairing_var)) {
    expr_heat_mat <- expr_heat_mat %>% dplyr::select(-c(!!sym(pairing_var)))
  }
  expr_heat_mat <- expr_heat_mat %>% select(-c(id))

  expr_heat_mat <- expr_heat_mat %>% dplyr::group_by(!!sym(group)) %>%
                    dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE))

  row_annot <- as.factor(expr_heat_mat[[grouping_var]])


  expr_heat_mat <- expr_heat_mat %>% select(-c(!!sym(grouping_var)))
  expr_heat_mat <- as.matrix(expr_heat_mat)
  rownames(expr_heat_mat) <- row_annot


  row_ha <- rowAnnotation(group = row_annot,
                          annotation_legend_param = list(
                            group = list(
                              ncol = 2,
                              title = "Group",
                              title_position = "topcenter",
                              at = names(group_cols),
                              grid_height = unit(0.02 * (nrow(expr_heat_mat) + 2), "cm"),
                              grid_width = unit(0.02 * (nrow(expr_heat_mat) + 2), 'cm'),
                              labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                              title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                            )
                          ),
                          col = list(group = group_cols),
                          na_col = "white",
                          show_annotation_name = FALSE
  )


  hm <- Heatmap(expr_heat_mat,
                cluster_rows = TRUE,
                cluster_columns = TRUE,
                row_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                column_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                right_annotation = row_ha,
                heatmap_legend_param = list(
                  title = "Cluster abundance [%]",
                  direction = 'horizontal',
                  title_position = "topcenter",
                  legend_width = unit(0.25 * (nrow(expr_heat_mat) + 2), "cm"),
                  grid_width = unit(0.25 * (nrow(expr_heat_mat) + 2), 'cm'),
                  labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                  title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                ))

  pdf(file = paste0(output_group, prefix, "_heatmap_per_group.pdf"),  width = 1.1 * ncol(expr_heat_mat), height = 3 + 0.5 * nrow(expr_heat_mat))
  draw(hm,
      heatmap_legend_side = 'top',
      row_sub_title_side = 'left',
      padding = unit(c(40, 0, 0, 40), "pt"))

  invisible(dev.off())


  #scaled per patient version
  expr_heat_mat <- data

  if (!is.null(pairing_var)) {
    expr_heat_mat <- expr_heat_mat %>% dplyr::select(-c(!!sym(pairing_var)))
  }
  expr_heat_mat <- expr_heat_mat %>% select(-c(id))

  expr_heat_mat <- expr_heat_mat %>% dplyr::group_by(!!sym(group)) %>%
                    dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE))

  row_annot <- as.factor(expr_heat_mat[[grouping_var]])


  expr_heat_mat <- expr_heat_mat %>% select(-c(!!sym(grouping_var)))
  expr_heat_mat <- as.matrix(expr_heat_mat)
  expr_heat_mat <- expr_heat_mat[, apply(expr_heat_mat, MARGIN = 2, FUN = is.numeric)]
  expr_heat_mat <- apply(expr_heat_mat, MARGIN = 2, FUN = scale)
  rownames(expr_heat_mat) <- row_annot

  row_ha <- rowAnnotation(group = row_annot,
                          annotation_legend_param = list(
                            group = list(
                              ncol = 2,
                              title = "Group",
                              title_position = "topcenter",
                              at = names(group_cols),
                              grid_height = unit(0.02 * (nrow(expr_heat_mat) + 2), "cm"),
                              grid_width = unit(0.02 * (nrow(expr_heat_mat) + 2), 'cm'),
                              labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                              title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                            )
                          ),
                          col = list(group = group_cols),
                          na_col = "white",
                          show_annotation_name = FALSE
  )


  hm <- Heatmap(expr_heat_mat,
                cluster_rows = TRUE,
                cluster_columns = TRUE,
                row_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                column_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                right_annotation = row_ha,
                heatmap_legend_param = list(
                  title = "Scaled cluster abundance",
                  direction = 'horizontal',
                  title_position = "topcenter",
                  legend_width = unit(0.25 * (nrow(expr_heat_mat) + 2), "cm"),
                  grid_width = unit(0.25 * (nrow(expr_heat_mat) + 2), 'cm'),
                  labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                  title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                ))

  pdf(file = paste0(output_group, prefix, "_heatmap_per_group_scaled.pdf"),  width = 1.1 * ncol(expr_heat_mat), height = 3 + 0.5 * nrow(expr_heat_mat))
  draw(hm,
      heatmap_legend_side = 'top',
      row_sub_title_side = 'left',
      padding = unit(c(40, 0, 0, 40), "pt"))

  invisible(dev.off())

  if (!is.null(custom_marker_order)) {
    if (all(custom_marker_order %in% colnames(data))) {
      if (!is.null(pairing_var)) {
        data <- data[, c(custom_marker_order, "id", grouping_var, pairing_var)]
      } else {
        data <- data[, c(custom_marker_order, "id", grouping_var)]
      }
    } else {
      warning("Some markers in custom_marker_order are not present in the data. Skipping custom ordering.")
      return(NULL)
    }

      
    # per patient version
    expr_heat_mat <- data

    row_annot <- as.factor(expr_heat_mat[[grouping_var]])
    id_annot <- expr_heat_mat[["id"]]

    if (!is.null(pairing_var)) {
      expr_heat_mat <- expr_heat_mat %>% dplyr::select(-c(!!sym(pairing_var)))
    }
    expr_heat_mat <- expr_heat_mat %>% select(-c(id, !!sym(grouping_var)))
    expr_heat_mat <- as.matrix(expr_heat_mat)
    rownames(expr_heat_mat) <- id_annot


    row_ha <- rowAnnotation(group = row_annot,
                            annotation_legend_param = list(
                              group = list(
                                ncol = 2,
                                title = "Group",
                                title_position = "topcenter",
                                at = names(group_cols),
                                grid_height = unit(0.02 * length(row_annot), "cm"),
                                grid_width = unit(0.02 * length(row_annot), 'cm'),
                                labels_gp = gpar(fontsize = 0.6 * length(row_annot)),
                                title_gp = gpar(fontsize = 0.6 * length(row_annot))
                              )
                            ),
                            col = list(group = group_cols),
                            na_col = "white",
                            show_annotation_name = FALSE
    )


    hm <- Heatmap(expr_heat_mat,
                  cluster_rows = TRUE,
                  cluster_columns = FALSE,
                  row_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                  column_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                  right_annotation = row_ha,
                  heatmap_legend_param = list(
                    title = "Mean Marker Expression",
                    direction = 'horizontal',
                    title_position = "topcenter",
                    legend_width = unit(0.25 * (nrow(expr_heat_mat) + 2), "cm"),
                    grid_width = unit(0.25 * (nrow(expr_heat_mat) + 2), 'cm'),
                    labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                    title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                  ))

    pdf(file = paste0(output_group, prefix, "_heatmap_per_sample_custom_order.pdf"), width = 1.1 * ncol(expr_heat_mat), height = 2 + 0.7 * nrow(expr_heat_mat))
    draw(hm,
        heatmap_legend_side = 'top',
        row_sub_title_side = 'left',
        padding = unit(c(40, 0, 0, 40), "pt"))

    invisible(dev.off())


    #scaled per patient version
    expr_heat_mat <- data

    row_annot <- as.factor(expr_heat_mat[[grouping_var]])
    id_annot <- expr_heat_mat[["id"]]


    if (!is.null(pairing_var)) {
      expr_heat_mat <- expr_heat_mat %>% dplyr::select(-c(!!sym(pairing_var)))
    }
    expr_heat_mat <- expr_heat_mat %>% select(-c(id, !!sym(grouping_var)))
    expr_heat_mat <- as.matrix(expr_heat_mat)
    #only keep numeric columns
    expr_heat_mat <- expr_heat_mat[, apply(expr_heat_mat, MARGIN = 2, FUN = is.numeric)]
    expr_heat_mat <- apply(expr_heat_mat, MARGIN = 2, FUN = scale)
    rownames(expr_heat_mat) <- id_annot


    row_ha <- rowAnnotation(group = row_annot,
                            annotation_legend_param = list(
                              group = list(
                                ncol = 2,
                                title = "Group",
                                title_position = "topcenter",
                                at = names(group_cols),
                                grid_height = unit(0.02 * (nrow(expr_heat_mat) + 2), "cm"),
                                grid_width = unit(0.02 * (nrow(expr_heat_mat) + 2), 'cm'),
                                labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                                title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                              )
                            ),
                            col = list(group = group_cols),
                            na_col = "white",
                            show_annotation_name = FALSE
    )


    hm <- Heatmap(expr_heat_mat,
                  cluster_rows = TRUE,
                  cluster_columns = FALSE,
                  row_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                  column_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                  right_annotation = row_ha,
                  heatmap_legend_param = list(
                    title = "Scaled cluster abundance",
                    direction = 'horizontal',
                    title_position = "topcenter",
                    legend_width = unit(0.25 * (nrow(expr_heat_mat) + 2), "cm"),
                    grid_width = unit(0.25 * (nrow(expr_heat_mat) + 2), 'cm'),
                    labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                    title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                  ))

    pdf(file = paste0(output_group, prefix, "_heatmap_per_sample_scaled_custom_order.pdf"), width = 1.1 * ncol(expr_heat_mat), height = 2 + 0.7 * nrow(expr_heat_mat))
    draw(hm,
        heatmap_legend_side = 'top',
        row_sub_title_side = 'left',
        padding = unit(c(40, 0, 0, 40), "pt"))

    invisible(dev.off())




    # per group version
    expr_heat_mat <- data

    if (!is.null(pairing_var)) {
      expr_heat_mat <- expr_heat_mat %>% dplyr::select(-c(!!sym(pairing_var)))
    }
    expr_heat_mat <- expr_heat_mat %>% select(-c(id))

    expr_heat_mat <- expr_heat_mat %>% dplyr::group_by(!!sym(group)) %>%
                      dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE))

    row_annot <- as.factor(expr_heat_mat[[grouping_var]])


    expr_heat_mat <- expr_heat_mat %>% select(-c(!!sym(grouping_var)))
    expr_heat_mat <- as.matrix(expr_heat_mat)
    rownames(expr_heat_mat) <- row_annot


    row_ha <- rowAnnotation(group = row_annot,
                            annotation_legend_param = list(
                              group = list(
                                ncol = 2,
                                title = "Group",
                                title_position = "topcenter",
                                at = names(group_cols),
                                grid_height = unit(0.02 * (nrow(expr_heat_mat) + 2), "cm"),
                                grid_width = unit(0.02 * (nrow(expr_heat_mat) + 2), 'cm'),
                                labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                                title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                              )
                            ),
                            col = list(group = group_cols),
                            na_col = "white",
                            show_annotation_name = FALSE
    )


    hm <- Heatmap(expr_heat_mat,
                  cluster_rows = TRUE,
                  cluster_columns = FALSE,
                  row_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                  column_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                  right_annotation = row_ha,
                  heatmap_legend_param = list(
                    title = "Cluster abundance [%]",
                    direction = 'horizontal',
                    title_position = "topcenter",
                    legend_width = unit(0.25 * (nrow(expr_heat_mat) + 2), "cm"),
                    grid_width = unit(0.25 * (nrow(expr_heat_mat) + 2), 'cm'),
                    labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                    title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                  ))

    pdf(file = paste0(output_group, prefix, "_heatmap_per_group_custom_order.pdf"),  width = 1.1 * ncol(expr_heat_mat), height = 3 + 0.5 * nrow(expr_heat_mat))
    draw(hm,
        heatmap_legend_side = 'top',
        row_sub_title_side = 'left',
        padding = unit(c(40, 0, 0, 40), "pt"))

    invisible(dev.off())


    #scaled per patient version
    expr_heat_mat <- data

    if (!is.null(pairing_var)) {
      expr_heat_mat <- expr_heat_mat %>% dplyr::select(-c(!!sym(pairing_var)))
    }
    expr_heat_mat <- expr_heat_mat %>% select(-c(id))

    expr_heat_mat <- expr_heat_mat %>% dplyr::group_by(!!sym(group)) %>%
                      dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE))

    row_annot <- as.factor(expr_heat_mat[[grouping_var]])


    expr_heat_mat <- expr_heat_mat %>% select(-c(!!sym(grouping_var)))
    expr_heat_mat <- as.matrix(expr_heat_mat)
    expr_heat_mat <- expr_heat_mat[, apply(expr_heat_mat, MARGIN = 2, FUN = is.numeric)]
    expr_heat_mat <- apply(expr_heat_mat, MARGIN = 2, FUN = scale)
    rownames(expr_heat_mat) <- row_annot

    row_ha <- rowAnnotation(group = row_annot,
                            annotation_legend_param = list(
                              group = list(
                                ncol = 2,
                                title = "Group",
                                title_position = "topcenter",
                                at = names(group_cols),
                                grid_height = unit(0.02 * (nrow(expr_heat_mat) + 2), "cm"),
                                grid_width = unit(0.02 * (nrow(expr_heat_mat) + 2), 'cm'),
                                labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                                title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                              )
                            ),
                            col = list(group = group_cols),
                            na_col = "white",
                            show_annotation_name = FALSE
    )


    hm <- Heatmap(expr_heat_mat,
                  cluster_rows = TRUE,
                  cluster_columns = FALSE,
                  row_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                  column_names_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 10)),
                  right_annotation = row_ha,
                  heatmap_legend_param = list(
                    title = "Scaled cluster abundance",
                    direction = 'horizontal',
                    title_position = "topcenter",
                    legend_width = unit(0.25 * (nrow(expr_heat_mat) + 2), "cm"),
                    grid_width = unit(0.25 * (nrow(expr_heat_mat) + 2), 'cm'),
                    labels_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2)),
                    title_gp = gpar(fontsize = 0.6 * (nrow(expr_heat_mat) + 2))
                  ))

    pdf(file = paste0(output_group, prefix, "_heatmap_per_group_scaled_custom_order.pdf"),  width = 1.1 * ncol(expr_heat_mat), height = 3 + 0.5 * nrow(expr_heat_mat))
    draw(hm,
        heatmap_legend_side = 'top',
        row_sub_title_side = 'left',
        padding = unit(c(40, 0, 0, 40), "pt"))

    invisible(dev.off())
  }

}





do_paired_boxplots <- function(data, testing_results = NULL, grouping_var = group, pairing_var = "id", 
                           features, group_by_clusters = TRUE, cluster_var = cluster_var, 
                           selected_clusters = NULL, column_number = 4, show_testing = TRUE,
                           show_pvalues = TRUE, show_outliers = TRUE, line_color = "#464646", 
                           line_size = 1, prefix = "paired", remove_unpaired = TRUE) {
  
  # Verify that we have paired data
  if(length(unique(data[[grouping_var]])) < 2) {
    stop("Need at least two groups for paired comparison")
  }
  
  # Check if pairing variable exists in the data
  if(!pairing_var %in% names(data)) {
    stop(paste("Pairing variable", pairing_var, "not found in data"))
  }
  
  # Verify each pair has values for all groups
  pair_counts <- data %>% 
    dplyr::group_by(!!sym(pairing_var)) %>% 
    dplyr::summarize(n_groups = n_distinct(!!sym(grouping_var)))
  
  if(any(pair_counts$n_groups < length(unique(data[[grouping_var]])))) {
    warning("Some pairs don't have values for all groups. Lines will only connect existing pairs.")
  }


  if (remove_unpaired == TRUE) {
    if (is.null(manual_comparisons)) {
      unique_groups <- unique(data[[grouping_var]])
      n_unique_groups <- length(unique(data[[grouping_var]]))
    } else {
      unique_groups <- manual_comparisons
      n_unique_groups <- length(manual_comparisons)
    }

    if (group_by_clusters == TRUE) {
      n_number <- data %>%
                  dplyr::group_by(!!sym(grouping_var), !!sym(cluster_var)) %>%
                  dplyr::summarise(n = n())
    } else {
      n_number <- data %>%
                  dplyr::group_by(!!sym(grouping_var)) %>%
                  dplyr::summarise(n = n())
    }

    
    if (paired == TRUE) {

      # remove entries where pairing var is not present in both groups

      no_pair <- data %>%
        dplyr::ungroup() %>%
        select(!!sym(pairing_var), !!sym(grouping_var)) %>%
        dplyr::distinct() %>%
        dplyr::group_by(!!sym(pairing_var)) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::filter(n < 2) %>%
        dplyr::pull(!!sym(pairing_var))

      data <- data[!unlist(data[, pairing_var]) %in% no_pair, ]
    }
  }
  
  if (group_by_clusters == TRUE && length(features) > 1) {
    stop("When grouping by clusters, only one feature can be plotted at a time.")
  } else if (group_by_clusters == FALSE && length(features) > 1) {
    data <- data %>%
      tidyr::pivot_longer(cols = all_of(features), names_to = "feature", values_to = "value")
    
    if (!is.null(selected_clusters)) {
      data <- data %>% filter(!!sym(cluster_var) %in% selected_clusters)
    }
    
    if (show_outliers == FALSE) {
      data <- data %>%
        group_by(feature, !!sym(grouping_var)) %>%
        dplyr::filter(
          value >= quantile(value, 0.25) - 1.5 * IQR(value) &
          value <= quantile(value, 0.75) + 1.5 * IQR(value)
        ) %>%
        ungroup()
    }
    
    p <- ggplot(data, aes(x = !!sym(grouping_var), y = value, color = !!sym(grouping_var))) +
      geom_boxplot(size = 2, outlier.shape = NA) +
      geom_point(size = 5, alpha = 0.7) +
      geom_line(aes(group = !!sym(pairing_var)), color = line_color, size = line_size) +
      xlab(grouping_var) +
      ylab('Marker Expression') +
      scale_color_manual(values = group_cols, limits = force) +
      facet_wrap(~ feature, scales = "free", ncol = column_number,
                 labeller = labeller(.cols = label_wrap_gen(width = 25))) +
      theme_cowplot() +
      theme(
        text = element_text(size = 45),
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 30, angle = 45, hjust = 1, vjust = 1, face = "plain"),
        axis.text.y = element_text(color = "black", size = 45, angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
        plot.margin = margin(t = 20, r = 0, b = 0, l = 10),
        strip.background = element_rect(fill = "#ffffff")
      )
    
    if (show_testing == TRUE && !is.null(testing_results)) {
      testing_results$feature <- testing_results$.y.
      if (show_pvalues == TRUE) {
        y_positions <- c()
        for (i in unique(testing_results[["feature"]])) {
          temp <- get_y_position(data[data$feature == i, ], reformulate(grouping_var, "value"), step.increase = 0.12, scales = "free")$y.position
          y_positions <- c(y_positions, temp)
        }
        testing_results$y.position <- y_positions
        testing_results$p.adj <- round(testing_results$p.adj, 3)
        p <- p + stat_pvalue_manual(testing_results, label = "p.adj", hide.ns = FALSE, size = 8, tip.length = 0.01)
      } else {
        y_positions <- c()
        for (i in unique(testing_results[["feature"]])) {
          temp <- get_y_position(data[data$feature == i, ], reformulate(grouping_var, "value"), step.increase = 0.12, scales = "free")$y.position
          y_positions <- c(y_positions, temp)
        }
        testing_results$y.position <- y_positions
        
        nrow_signif <- testing_results %>%
          dplyr::group_by(.data[["feature"]]) %>%
          dplyr::summarise(n_signif = sum(p.adj.signif != "ns"))
        
        if (sum(nrow_signif$n_signif > 0) > 0) {
          filtered_testing <- c()
          for (i in unique(testing_results[["feature"]])) {
            marker_nrow_signif <- nrow_signif$n_signif[nrow_signif[["feature"]] == i]
            if (marker_nrow_signif > 0) {
              temp_y_positions <- testing_results$y.position[testing_results[["feature"]] == i]
              temp_testing <- testing_results %>% dplyr::filter(.data[["feature"]] == i & p.adj.signif != "ns")
              temp_testing$y.position <- temp_y_positions[1:marker_nrow_signif]
              filtered_testing <- rbind(filtered_testing, temp_testing)
            }
          }
          testing_results <- filtered_testing
          p <- p + stat_pvalue_manual(testing_results, label = "p.adj.signif", hide.ns = FALSE, size = 15, tip.length = 0.01)
        }
      }
    }
    
    n_features <- length(features)
    pdf(paste0(output_group, prefix, "_", "paired_boxplot_grid", ".pdf"),
        width = 8 * column_number,
        height = 12 * ceiling(n_features / column_number)
    )
    print(p)
    invisible(dev.off())
    
    singles_output <- paste0(output_group, prefix, "_singles/")
    dir.create(singles_output, showWarnings = FALSE)
    
    for (facet in unique(data$feature)) {
      data_subset <- data[data$feature == facet, ]
      
      p_subset <- ggplot(data_subset, aes(x = !!sym(grouping_var), y = value, color = !!sym(grouping_var))) +
        geom_boxplot(size = 2, outlier.shape = NA) +
        geom_point(size = 5, alpha = 0.7) +
        geom_line(aes(group = !!sym(pairing_var)), color = line_color, size = line_size) +
        ggtitle(paste(facet)) +
        xlab(grouping_var) +
        ylab('Marker Expression') +
        scale_color_manual(values = group_cols, limits = force) +
        theme_cowplot() +
        theme(
          text = element_text(size = 45),
          legend.position = "none",
          axis.text.x = element_text(color = "black", size = 30, angle = 45, hjust = 1, vjust = 1, face = "plain"),
          axis.text.y = element_text(color = "black", size = 45, angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
          plot.margin = margin(t = 20, r = 0, b = 0, l = 10),
          strip.background = element_rect(fill = "#ffffff")
        )
      
      if (show_testing == TRUE && !is.null(testing_results)) {
        if (show_pvalues == TRUE) {
          feature_testing <- testing_results[testing_results[["feature"]] == facet, ]
          if(nrow(feature_testing) > 0) {
            p_subset <- p_subset + stat_pvalue_manual(feature_testing, label = "p.adj", hide.ns = FALSE, size = 8, tip.length = 0.01)
          }
        } else if(exists("filtered_testing")) {
          feature_testing <- filtered_testing[filtered_testing[["feature"]] == facet, ]
          if(nrow(feature_testing) > 0) {
            p_subset <- p_subset + stat_pvalue_manual(feature_testing, label = "p.adj.signif", hide.ns = FALSE, size = 8, tip.length = 0.01)
          }
        }
      }
      
      pdf(paste0(singles_output, prefix, "_", "paired_boxplot_", facet, ".pdf"),
          width = 8,
          height = 12
      )
      print(p_subset)
      invisible(dev.off())
    }
    
  } else if (group_by_clusters == TRUE && length(features) == 1) {
    data <- data %>%
      tidyr::pivot_longer(cols = all_of(features), names_to = "feature", values_to = "value")
    
    if (!is.null(selected_clusters)) {
      data <- data %>% filter(!!sym(cluster_var) %in% selected_clusters)
    }
    
    if (show_outliers == FALSE) {
      data <- data %>%
        group_by(!!sym(cluster_var), !!sym(grouping_var)) %>%
        dplyr::filter(
          value >= quantile(value, 0.25) - 1.5 * IQR(value) &
          value <= quantile(value, 0.75) + 1.5 * IQR(value)
        ) %>%
        ungroup()
    }
    
    p <- ggplot(data, aes(x = !!sym(grouping_var), y = value, color = !!sym(grouping_var))) +
      geom_boxplot(size = 2, outlier.shape = NA) +
      geom_point(size = 5, alpha = 0.7) +
      geom_line(aes(group = !!sym(pairing_var)), color = line_color, size = line_size) +
      xlab(grouping_var) +
      ylab('Cluster Abundance [%]') +
      scale_color_manual(values = group_cols, limits = force) +
      facet_wrap(~ .data[[cluster_var]], scales = "free", ncol = column_number,
                 labeller = labeller(.cols = label_wrap_gen(width = 25))) +
      theme_cowplot() +
      theme(
        text = element_text(size = 45),
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 30, angle = 45, hjust = 1, vjust = 1, face = "plain"),
        axis.text.y = element_text(color = "black", size = 45, angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
        plot.margin = margin(t = 20, r = 0, b = 0, l = 10),
        strip.background = element_rect(fill = "#ffffff")
      )
    
    if (show_testing == TRUE && !is.null(testing_results)) {
      if (show_pvalues == TRUE) {
        y_positions <- c()
        for (i in unique(testing_results[[cluster_var]])) {
          temp <- get_y_position(data[data[[cluster_var]] == i, ], reformulate(grouping_var, "value"), step.increase = 0.12, scales = "free")$y.position
          y_positions <- c(y_positions, temp)
        }
        testing_results$y.position <- y_positions
        testing_results$p.adj <- round(testing_results$p.adj, 3)
        p <- p + stat_pvalue_manual(testing_results, label = "p.adj", hide.ns = FALSE, size = 8, tip.length = 0.01)
        
      } else {
        y_positions <- c()
        for (i in unique(testing_results[[cluster_var]])) {
          temp <- get_y_position(data[data[[cluster_var]] == i, ], reformulate(grouping_var, "value"), step.increase = 0.12, scales = "free")$y.position
          y_positions <- c(y_positions, temp)
        }
        testing_results$y.position <- y_positions
        
        nrow_signif <- testing_results %>%
          dplyr::group_by(.data[[cluster_var]]) %>%
          dplyr::summarise(n_signif = sum(p.adj.signif != "ns"))
        
        if (sum(nrow_signif$n_signif > 0) > 0) {
          filtered_testing <- c()
          for (i in unique(testing_results[[cluster_var]])) {
            cluster_nrow_signif <- nrow_signif$n_signif[nrow_signif[[cluster_var]] == i]
            if (cluster_nrow_signif > 0) {
              temp_y_positions <- testing_results$y.position[testing_results[[cluster_var]] == i]
              temp_testing <- testing_results %>% dplyr::filter(.data[[cluster_var]] == i & p.adj.signif != "ns")
              temp_testing$y.position <- temp_y_positions[1:cluster_nrow_signif]
              filtered_testing <- rbind(filtered_testing, temp_testing)
            }
          }
          testing_results <- filtered_testing
          p <- p + stat_pvalue_manual(testing_results, label = "p.adj.signif", hide.ns = FALSE, size = 15, tip.length = 0.01)
        }
      }
    }
    
    n_clusters <- length(unique(data[[cluster_var]]))
    pdf(paste0(output_group, prefix, "_", "paired_boxplot_grid", ".pdf"),
        width = 8 * column_number,
        height = 12 * ceiling(n_clusters / column_number)
    )
    print(p)
    invisible(dev.off())
    
    singles_output <- paste0(output_group, prefix, "_singles/")
    dir.create(singles_output, showWarnings = FALSE)
    
    for (facet in unique(data[[cluster_var]])) {
      data_subset <- data[data[[cluster_var]] == facet, ]
      
      p_subset <- ggplot(data_subset, aes(x = !!sym(grouping_var), y = value, color = !!sym(grouping_var))) +
        geom_boxplot(size = 2, outlier.shape = NA) +
        geom_point(size = 5, alpha = 0.7) +
        geom_line(aes(group = !!sym(pairing_var)), color = line_color, size = line_size) +
        ggtitle(paste(facet)) +
        xlab(grouping_var) +
        ylab('Cluster Abundance [%]') +
        scale_color_manual(values = group_cols, limits = force) +
        theme_cowplot() +
        theme(
          text = element_text(size = 45),
          legend.position = "none",
          axis.text.x = element_text(color = "black", size = 30, angle = 45, hjust = 1, vjust = 1, face = "plain"),
          axis.text.y = element_text(color = "black", size = 45, angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
          plot.margin = margin(t = 20, r = 0, b = 0, l = 10),
          strip.background = element_rect(fill = "#ffffff")
        )
      
      if (show_testing == TRUE && !is.null(testing_results)) {
        if (show_pvalues == TRUE) {
          cluster_testing <- testing_results[testing_results[[cluster_var]] == facet, ]
          if(nrow(cluster_testing) > 0) {
            p_subset <- p_subset + stat_pvalue_manual(cluster_testing, label = "p.adj", hide.ns = FALSE, size = 8, tip.length = 0.01)
          }
        } else if(exists("filtered_testing")) {
          cluster_testing <- filtered_testing[filtered_testing[[cluster_var]] == facet, ]
          if(nrow(cluster_testing) > 0) {
            p_subset <- p_subset + stat_pvalue_manual(cluster_testing, label = "p.adj.signif", hide.ns = FALSE, size = 8, tip.length = 0.01)
          }
        }
      }
      
      pdf(paste0(singles_output, prefix, "_", "paired_boxplot_", facet, ".pdf"),
          width = 8,
          height = 12
      )
      print(p_subset)
      invisible(dev.off())
    }
  }
  
  return(p)
}
