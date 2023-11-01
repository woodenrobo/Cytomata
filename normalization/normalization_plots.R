exploration_ridges <- function() {
  cat(paste0("Plotting ", channel, " exploration ridges\n"))

  percentile_colors <- c("p60" = "#00acfc", "p70"="#00acfc", "p75"="#00acfc", "p80"="#00acfc", "p85"="#00acfc", "p90"="#00acfc", "p95"="#00acfc",
                          "p97"="#00acfc", "p99"="#00acfc", "p99_5"="#00acfc", "p99_9"="#00acfc")

  percentile_sizes <- c("p60" = 1, "p70"= 1, "p75"= 1, "p80"= 1, "p85"= 1, "p90"= 1, "p95"= 1,
                          "p97"= 1, "p99"= 1, "p99_5"= 1, "p99_9"= 1)
  
  ridges_fills <- c("no" = "#52688a", "global" = "#b98628", "local" = "#9f2121")
  #ggridges method


  temp_set <- exprs_set[, c("sample", channel)]

  if (asinh_transform == FALSE) {

    #transform the expression values
    temp_set[, colnames(temp_set) %in% feature_markers] <- asinh(temp_set[, colnames(temp_set) %in% feature_markers] / cofac)
    cat("Applied arcsinh transformation with the cofactor of", cofac, "\n")

  }

    #quantile values for each anchor sample
    for (a_sample in unique(temp_set$sample)){
        p60 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.6)
        temp_set$p60[temp_set$sample == a_sample] <- p60
        p70 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.7)
        temp_set$p70[temp_set$sample == a_sample] <- p70
        p75 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.75)
        temp_set$p75[temp_set$sample == a_sample] <- p75
        p80 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.8)
        temp_set$p80[temp_set$sample == a_sample] <- p80
        p85 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.85)
        temp_set$p85[temp_set$sample == a_sample] <- p85
        p90 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.90)
        temp_set$p90[temp_set$sample == a_sample] <- p90
        p95 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.95)
        temp_set$p95[temp_set$sample == a_sample] <- p95
        p97 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.97)
        temp_set$p97[temp_set$sample == a_sample] <- p97
        p99 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.99)
        temp_set$p99[temp_set$sample == a_sample] <- p99
        p99_5 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.995)
        temp_set$p99_5[temp_set$sample == a_sample] <- p99_5
        p99_9 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.999)
        temp_set$p99_9[temp_set$sample == a_sample] <- p99_9
    }


  #temp_set$optimal_anchor[temp_set$sample == optimal_global_anchor] <- "global"
  temp_local_optimal_anchor <- optimal_anchor[optimal_anchor$channel == channel, ]
  temp_set$optimal_anchor[temp_set$sample == temp_local_optimal_anchor$best_anchor] <- "local"
  temp_set$optimal_anchor[is.na(temp_set$optimal_anchor)] <- "no"

  temp_opt_perc <- optimal_percentile[optimal_percentile$channel == channel, "percentile"]
  percentile_colors[names(percentile_colors) %in% temp_opt_perc] <- "#c70000"
  percentile_sizes[names(percentile_sizes) %in% temp_opt_perc] <- 2
  #temp_set <- temp_set[temp_set[, channel] > 0, ]
  temp_set <- temp_set[sample(nrow(temp_set), round(nrow(temp_set) * downsampling_factor), replace = FALSE), ]
  temp_set$sample <- factor(temp_set$sample, ordered=T, levels=c(rev(unique(temp_set$sample)[order(unique(temp_set$sample))])))

  setwd(out_norm_dens_folder)
  png(filename = paste0(date, "_", project_name, "_", channel, "_ridges.png"), width = 1200, height = length(unique(temp_set$sample)) * 100)
  suppressMessages(
  print(  
  ggplot(temp_set, aes(x = temp_set[, channel], y = sample, fill = optimal_anchor))+
      geom_density_ridges2(scale = 1, alpha = 0.4, quantile_lines = FALSE, quantiles = c(0.6, 0.7, 0.75, 0.8, 0.85, 0.90, 0.95, 0.97, 0.99, 0.995, 0.999))+
      geom_line(data=temp_set, mapping=aes(x=p60, group = 1, color = "p60", size = "p60"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p60, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p70, group = 1, color = "p70", size = "p70"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p70, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p75, group = 1, color = "p75", size = "p75"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p75, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p80, group = 1, color = "p80", size = "p80"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p80, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p85, group = 1, color = "p85", size = "p85"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p85, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p90, group = 1, color = "p90", size = "p90"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p90, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p95, group = 1, color = "p95", size = "p95"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p95, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p97, group = 1, color = "p97", size = "p97"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p97, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p99, group = 1, color = "p99", size = "p99"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p99, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p99_5, group = 1, color = "p99_5", size = "p99_5"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p99_5, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p99_9, group = 1, color = "p99_9", size = "p99_9"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p99_9, group = 1), color = "black" )+
      scale_color_manual(values = percentile_colors) +
      scale_fill_manual(values = ridges_fills) +
      scale_size_manual(values = percentile_sizes) +
      theme_cowplot()+
      theme(axis.title.x = element_text(size = 45),
          axis.text.x = element_text(size = 35),
          axis.text.y = element_text(size = 25)
          )+
      xlab(as.character(channel))+
      ggtitle("OPTIMAL PERCENTILE LINE IS COLORED RED")
  )
  )
  invisible(dev.off())
}
    
exploration_ridges_wo_zeroes <- function() {
  cat(paste0("Plotting ", channel, " exploration ridges\n"))

  percentile_colors <- c("p60" = "#00acfc", "p70"="#00acfc", "p75"="#00acfc", "p80"="#00acfc", "p85"="#00acfc", "p90"="#00acfc", "p95"="#00acfc",
                          "p97"="#00acfc", "p99"="#00acfc", "p99_5"="#00acfc", "p99_9"="#00acfc")

  percentile_sizes <- c("p60" = 1, "p70"= 1, "p75"= 1, "p80"= 1, "p85"= 1, "p90"= 1, "p95"= 1,
                          "p97"= 1, "p99"= 1, "p99_5"= 1, "p99_9"= 1)
  
  ridges_fills <- c("no" = "#52688a", "global" = "#b98628", "local" = "#9f2121")
  #ggridges method


  temp_set <- exprs_set[, c("sample", channel)]

  if (asinh_transform == FALSE) {

    #transform the expression values
    temp_set[, colnames(temp_set) %in% feature_markers] <- asinh(temp_set[, colnames(temp_set) %in% feature_markers] / cofac)
    cat("Applied arcsinh transformation with the cofactor of", cofac, "\n")

  }
  
    #quantile values for each anchor sample
    for (a_sample in unique(temp_set$sample)){
        p60 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.6)
        temp_set$p60[temp_set$sample == a_sample] <- p60
        p70 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.7)
        temp_set$p70[temp_set$sample == a_sample] <- p70
        p75 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.75)
        temp_set$p75[temp_set$sample == a_sample] <- p75
        p80 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.8)
        temp_set$p80[temp_set$sample == a_sample] <- p80
        p85 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.85)
        temp_set$p85[temp_set$sample == a_sample] <- p85
        p90 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.90)
        temp_set$p90[temp_set$sample == a_sample] <- p90
        p95 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.95)
        temp_set$p95[temp_set$sample == a_sample] <- p95
        p97 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.97)
        temp_set$p97[temp_set$sample == a_sample] <- p97
        p99 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.99)
        temp_set$p99[temp_set$sample == a_sample] <- p99
        p99_5 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.995)
        temp_set$p99_5[temp_set$sample == a_sample] <- p99_5
        p99_9 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.999)
        temp_set$p99_9[temp_set$sample == a_sample] <- p99_9
    }


  #temp_set$optimal_anchor[temp_set$sample == optimal_global_anchor] <- "global"
  temp_local_optimal_anchor <- optimal_anchor[optimal_anchor$channel == channel, ]
  temp_set$optimal_anchor[temp_set$sample == temp_local_optimal_anchor$best_anchor] <- "local"
  temp_set$optimal_anchor[is.na(temp_set$optimal_anchor)] <- "no"


  temp_opt_perc <- optimal_percentile[optimal_percentile$channel == channel, "percentile"]
  percentile_colors[names(percentile_colors) %in% temp_opt_perc] <- "#c70000"
  percentile_sizes[names(percentile_sizes) %in% temp_opt_perc] <- 2

  temp_set <- temp_set[sample(nrow(temp_set), round(nrow(temp_set) * downsampling_factor), replace = FALSE), ]
  temp_set <- temp_set[temp_set[, channel] > 0, ]
  temp_set$sample <- factor(temp_set$sample, ordered=T, levels=c(rev(unique(temp_set$sample)[order(unique(temp_set$sample))])))

  setwd(out_norm_dens_folder)
  png(filename = paste0(date, "_", project_name, "_", channel, "_no_zeroes_ridges.png"), width = 1200, height = length(unique(temp_set$sample)) * 100)
  suppressMessages(
  print(  
  ggplot(temp_set, aes(x = temp_set[, channel], y = sample, fill = optimal_anchor)) +
      geom_density_ridges2(scale = 1, alpha = 0.4, quantile_lines = FALSE, quantiles = c(0.6, 0.7, 0.75, 0.8, 0.85, 0.90, 0.95, 0.97, 0.99, 0.995, 0.999)) +
      geom_line(data=temp_set, mapping=aes(x=p60, group = 1, color = "p60", size = "p60"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p60, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p70, group = 1, color = "p70", size = "p70"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p70, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p75, group = 1, color = "p75", size = "p75"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p75, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p80, group = 1, color = "p80", size = "p80"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p80, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p85, group = 1, color = "p85", size = "p85"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p85, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p90, group = 1, color = "p90", size = "p90"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p90, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p95, group = 1, color = "p95", size = "p95"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p95, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p97, group = 1, color = "p97", size = "p97"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p97, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p99, group = 1, color = "p99", size = "p99"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p99, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p99_5, group = 1, color = "p99_5", size = "p99_5"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p99_5, group = 1), color = "black" ) +
      geom_line(data=temp_set, mapping=aes(x=p99_9, group = 1, color = "p99_9", size = "p99_9"), orientation = "y",  alpha=0.7) +
      geom_point(data=temp_set, mapping=aes(x=p99_9, group = 1), color = "black" ) +
      scale_color_manual(values = percentile_colors) +
      scale_fill_manual(values = ridges_fills) +
      scale_size_manual(values = percentile_sizes) +
      theme_cowplot() +
      theme(axis.title.x = element_text(size = 45),
          axis.text.x = element_text(size = 35),
          axis.text.y = element_text(size = 25)
          ) +
      xlab(as.character(channel))+
      ggtitle("OPTIMAL PERCENTILE LINE IS COLORED RED")
  )
  )
  invisible(dev.off())
}


scaling_factors_barplots <- function() {

    #plot scaling factors in a faceted barplot
    ceiling_dec <- function(x, level=1){ round(x + 5*10^(-level-1), level)}

    filtered_factors$a_sample <- factor(filtered_factors$a_sample, ordered=T, levels=c(rev(unique(filtered_factors$a_sample)[order(unique(filtered_factors$a_sample))])))

    setwd(out_norm_aid_folder)
    png(filename = paste0("scaling_factors.png"), width = 700 + 250 * 5, 
        height = ceiling(length(unique(feature_markers)) / 5) * 35 + ceiling(length(unique(feature_markers)) / 5) * (40 * length(unique(exprs_set$sample))))
    print(ggplot(data = filtered_factors, aes(x = a_sample, y = as.numeric(factor))) +
            geom_bar(aes(fill = as.numeric(factor)), stat = "identity") +
            theme_cowplot() +
            theme(axis.title.x = element_text(size = 35),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                strip.text.x = element_text(size = 35),
                panel.spacing=unit(2,"lines")) +
            facet_wrap(~factor(filtered_factors$channel, ordered = TRUE, levels = unique(filtered_factors$channel)), ncol = 5, scales = "free_x") +
            scale_y_continuous(breaks = scales::pretty_breaks()) +
            scale_fill_gradientn(values = c(scales::rescale(min(as.numeric(filtered_factors$factor)), to = c(0, 1), from = c(0,ceiling_dec(max(as.numeric(filtered_factors$factor), level = 1)))),
                                                            scales::rescale(1, to = c(0, 1), from = c(0, ceiling_dec(max(as.numeric(filtered_factors$factor), level = 1)))),
                                            1),
                                    colours=c("blue", "darkgreen", "red"),
                                    limits = c(0, ceiling_dec(max(as.numeric(filtered_factors$factor), level = 1)))) +
            coord_flip() +
            xlab("Batch anchor") +
            ylab("Scaling factor")
    )
    invisible(dev.off())
}    

diagnostics_ridges <- function() {
  cat(paste0("Plotting ", channel, " diagnostic ridges\n"))

  percentile_colors <- c("p60" = "#00acfc", "p70"="#00acfc", "p75"="#00acfc", "p80"="#00acfc", "p85"="#00acfc", "p90"="#00acfc", "p95"="#00acfc",
                          "p97"="#00acfc", "p99"="#00acfc", "p99_5"="#00acfc", "p99_9"="#00acfc")

  percentile_sizes <- c("p60" = 1, "p70"= 1, "p75"= 1, "p80"= 1, "p85"= 1, "p90"= 1, "p95"= 1,
                          "p97"= 1, "p99"= 1, "p99_5"= 1, "p99_9"= 1)
  
  ridges_colors <- c("no" = "#52688a", "global" = "#b98628", "local" = "#9f2121")
  ridges_fills <- c("BEFORE" = "#aa9696", "AFTER" = "#8f3ad9")
  #ggridges method


  temp_set <- exprs_set[, c("sample", "norm_state", channel)]


  if (asinh_transform == FALSE) {

    #transform the expression values
    temp_set[, colnames(temp_set) %in% feature_markers] <- asinh(temp_set[, colnames(temp_set) %in% feature_markers] / cofac)
    cat("Applied arcsinh transformation with the cofactor of", cofac, "\n")

  }

    #temp_set$optimal_anchor[temp_set$sample == optimal_global_anchor] <- "global"
    temp_local_optimal_anchor <- optimal_anchor[optimal_anchor$channel == channel, ]
    temp_set$optimal_anchor[temp_set$sample == temp_local_optimal_anchor$best_anchor] <- "local"
    temp_set$optimal_anchor[is.na(temp_set$optimal_anchor)] <- "no"

    temp_opt_perc <- optimal_percentile[optimal_percentile$channel == channel, "percentile"]
    percentile_colors[names(percentile_colors) %in% temp_opt_perc] <- "#c70000"
    percentile_sizes[names(percentile_sizes) %in% temp_opt_perc] <- 2

    temp_set$sample <- paste0(temp_set$sample, "_", temp_set$norm_state)
    temp_set$sample <- factor(temp_set$sample, ordered=T, levels=c(rev(unique(temp_set$sample))))

    #quantile values for each anchor sample
    for (a_sample in unique(temp_set$sample)){
        p60 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.6)
        temp_set$p60[temp_set$sample == a_sample] <- p60
        p70 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.7)
        temp_set$p70[temp_set$sample == a_sample] <- p70
        p75 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.75)
        temp_set$p75[temp_set$sample == a_sample] <- p75
        p80 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.8)
        temp_set$p80[temp_set$sample == a_sample] <- p80
        p85 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.85)
        temp_set$p85[temp_set$sample == a_sample] <- p85
        p90 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.90)
        temp_set$p90[temp_set$sample == a_sample] <- p90
        p95 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.95)
        temp_set$p95[temp_set$sample == a_sample] <- p95
        p97 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.97)
        temp_set$p97[temp_set$sample == a_sample] <- p97
        p99 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.99)
        temp_set$p99[temp_set$sample == a_sample] <- p99
        p99_5 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.995)
        temp_set$p99_5[temp_set$sample == a_sample] <- p99_5
        p99_9 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.999)
        temp_set$p99_9[temp_set$sample == a_sample] <- p99_9
    }


  
  #temp_set <- temp_set[temp_set[, channel] > 0, ]
  temp_set <- temp_set[sample(nrow(temp_set), round(nrow(temp_set) * downsampling_factor), replace = FALSE), ]


  setwd(out_norm_dens_diag_folder)
  png(filename = paste0(date, "_", project_name, "_", channel, "_diagnostic_ridges.png"), width = 1350, height = length(unique(temp_set$sample)) * 100)
  suppressMessages(
  print(  
  ggplot(temp_set, aes(x = temp_set[, channel], y = sample, fill = norm_state, color = optimal_anchor))+
      geom_density_ridges2(scale = 1, alpha = 0.4, size = 1.5, quantile_lines = FALSE, quantiles = c(0.6, 0.7, 0.75, 0.8, 0.85, 0.90, 0.95, 0.97, 0.99, 0.995, 0.999))+
      scale_color_manual(values = ridges_colors) +
      new_scale_color() +
      geom_line(data=temp_set, mapping=aes(x=p60, group = 1, color = "p60", size = "p60"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p60, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p70, group = 1, color = "p70", size = "p70"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p70, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p75, group = 1, color = "p75", size = "p75"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p75, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p80, group = 1, color = "p80", size = "p80"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p80, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p85, group = 1, color = "p85", size = "p85"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p85, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p90, group = 1, color = "p90", size = "p90"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p90, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p95, group = 1, color = "p95", size = "p95"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p95, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p97, group = 1, color = "p97", size = "p97"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p97, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p99, group = 1, color = "p99", size = "p99"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p99, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p99_5, group = 1, color = "p99_5", size = "p99_5"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p99_5, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p99_9, group = 1, color = "p99_9", size = "p99_9"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p99_9, group = 1), color = "black" )+
      scale_color_manual(values = percentile_colors) +
      scale_fill_manual(values = ridges_fills) +
      scale_size_manual(values = percentile_sizes) +
      theme_cowplot()+
      theme(axis.title.x = element_text(size = 45),
          axis.text.x = element_text(size = 35),
          axis.text.y = element_text(size = 25)
          )+
      xlab(as.character(channel))+
      ggtitle("OPTIMAL PERCENTILE LINE IS COLORED RED")
  )
  )
  invisible(dev.off())
}

diagnostics_ridges_wo_zeroes <- function() {
  cat(paste0("Plotting ", channel, " diagnostic ridges\n"))

  percentile_colors <- c("p60" = "#00acfc", "p70"="#00acfc", "p75"="#00acfc", "p80"="#00acfc", "p85"="#00acfc", "p90"="#00acfc", "p95"="#00acfc",
                          "p97"="#00acfc", "p99"="#00acfc", "p99_5"="#00acfc", "p99_9"="#00acfc")

  percentile_sizes <- c("p60" = 1, "p70"= 1, "p75"= 1, "p80"= 1, "p85"= 1, "p90"= 1, "p95"= 1,
                          "p97"= 1, "p99"= 1, "p99_5"= 1, "p99_9"= 1)
  
  ridges_colors <- c("no" = "#52688a", "global" = "#b98628", "local" = "#9f2121")
  ridges_fills <- c("BEFORE" = "#aa9696", "AFTER" = "#8f3ad9")
  #ggridges method


  temp_set <- exprs_set[, c("sample", "norm_state", channel)]


  if (asinh_transform == FALSE) {

    #transform the expression values
    temp_set[, colnames(temp_set) %in% feature_markers] <- asinh(temp_set[, colnames(temp_set) %in% feature_markers] / cofac)
    cat("Applied arcsinh transformation with the cofactor of", cofac, "\n")

  }

    #temp_set$optimal_anchor[temp_set$sample == optimal_global_anchor] <- "global"
    temp_local_optimal_anchor <- optimal_anchor[optimal_anchor$channel == channel, ]
    temp_set$optimal_anchor[temp_set$sample == temp_local_optimal_anchor$best_anchor] <- "local"
    temp_set$optimal_anchor[is.na(temp_set$optimal_anchor)] <- "no"

    temp_opt_perc <- optimal_percentile[optimal_percentile$channel == channel, "percentile"]
    percentile_colors[names(percentile_colors) %in% temp_opt_perc] <- "#c70000"
    percentile_sizes[names(percentile_sizes) %in% temp_opt_perc] <- 2

    temp_set$sample <- paste0(temp_set$sample, "_", temp_set$norm_state)
    temp_set$sample <- factor(temp_set$sample, ordered=T, levels=c(rev(unique(temp_set$sample))))

    #quantile values for each anchor sample
    for (a_sample in unique(temp_set$sample)){
        p60 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.6)
        temp_set$p60[temp_set$sample == a_sample] <- p60
        p70 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.7)
        temp_set$p70[temp_set$sample == a_sample] <- p70
        p75 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.75)
        temp_set$p75[temp_set$sample == a_sample] <- p75
        p80 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.8)
        temp_set$p80[temp_set$sample == a_sample] <- p80
        p85 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.85)
        temp_set$p85[temp_set$sample == a_sample] <- p85
        p90 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.90)
        temp_set$p90[temp_set$sample == a_sample] <- p90
        p95 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.95)
        temp_set$p95[temp_set$sample == a_sample] <- p95
        p97 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.97)
        temp_set$p97[temp_set$sample == a_sample] <- p97
        p99 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.99)
        temp_set$p99[temp_set$sample == a_sample] <- p99
        p99_5 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.995)
        temp_set$p99_5[temp_set$sample == a_sample] <- p99_5
        p99_9 <- quantile(temp_set[temp_set$sample == a_sample, channel], 0.999)
        temp_set$p99_9[temp_set$sample == a_sample] <- p99_9
    }


  
  temp_set <- temp_set[temp_set[, channel] > 0, ]
  temp_set <- temp_set[sample(nrow(temp_set), round(nrow(temp_set) * downsampling_factor), replace = FALSE), ]


  setwd(out_norm_dens_diag_folder)
  png(filename = paste0(date, "_", project_name, "_", channel, "_diagnostic_ridges.png"), width = 1350, height = length(unique(temp_set$sample)) * 100)
  suppressMessages(
  print(  
  ggplot(temp_set, aes(x = temp_set[, channel], y = sample, fill = norm_state, color = optimal_anchor))+
      geom_density_ridges2(scale = 1, alpha = 0.4, size = 1.5, quantile_lines = FALSE, quantiles = c(0.6, 0.7, 0.75, 0.8, 0.85, 0.90, 0.95, 0.97, 0.99, 0.995, 0.999))+
      scale_color_manual(values = ridges_colors) +
      new_scale_color() +
      geom_line(data=temp_set, mapping=aes(x=p60, group = 1, color = "p60", size = "p60"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p60, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p70, group = 1, color = "p70", size = "p70"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p70, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p75, group = 1, color = "p75", size = "p75"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p75, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p80, group = 1, color = "p80", size = "p80"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p80, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p85, group = 1, color = "p85", size = "p85"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p85, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p90, group = 1, color = "p90", size = "p90"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p90, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p95, group = 1, color = "p95", size = "p95"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p95, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p97, group = 1, color = "p97", size = "p97"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p97, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p99, group = 1, color = "p99", size = "p99"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p99, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p99_5, group = 1, color = "p99_5", size = "p99_5"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p99_5, group = 1), color = "black" )+
      geom_line(data=temp_set, mapping=aes(x=p99_9, group = 1, color = "p99_9", size = "p99_9"), orientation = "y",  alpha=0.7)+
      geom_point(data=temp_set, mapping=aes(x=p99_9, group = 1), color = "black" )+
      scale_color_manual(values = percentile_colors) +
      scale_fill_manual(values = ridges_fills) +
      scale_size_manual(values = percentile_sizes) +
      theme_cowplot()+
      theme(axis.title.x = element_text(size = 45),
          axis.text.x = element_text(size = 35),
          axis.text.y = element_text(size = 25)
          )+
      xlab(as.character(channel))+
      ggtitle("OPTIMAL PERCENTILE LINE IS COLORED RED")
  )
  )
  invisible(dev.off())
}


diagnostics_mean_boxplots <- function() {
  col_number <- 5
  box_colors <- c("BEFORE" = "#705252", "AFTER" = "#592389")
  before_after_means$norm_state <- factor(before_after_means$norm_state, ordered=T, levels=c("BEFORE", "AFTER"))
  print(
  ggplot(before_after_means, aes(x = norm_state, y = means, color = norm_state)) +
    geom_boxplot( size = 0.7, outlier.shape = NA) +
    geom_jitter( size = 1.2, height = 0, width = 0.1)+
    facet_wrap(~ channel, ncol = col_number, scales = "free") + 
    scale_color_manual(values = box_colors) +
    theme_cowplot() +
    theme(text=element_text(size=10),
                            legend.position="none",
                            axis.text.x = element_text(color = "black", size = 10,
                                                       angle = 45, hjust = 1, vjust = 1, face = "plain"),
                            axis.text.y = element_text(color = "black", size = 10,
                                                       angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
                            axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 5, l = 0, unit = "pt")),
                            axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 10, unit = "pt")),
                            plot.margin = margin(t = 0, r = 10, b = 10, l = 0, unit = "pt")
    ) +
    labs(x = "Normalization state", y = "Mean signal intensity")
  )
  ggsave(paste0(date, "_", project_name, "_channel_means_before_after.pdf"), 
         plot = last_plot(), device = "pdf", path = out_norm_aid_diag_folder, 
         scale = 1, width = 0.16*col_number*length(unique(before_after_means$sample)), height = 2.2*ceiling(length(unique(before_after_means$channel))/col_number))
}

diagnostics_mean_barplots <- function() {
  col_number <- 5
  bar_fills <- c("BEFORE" = "#705252", "AFTER" = "#592389")
  before_after_means$norm_state <- factor(before_after_means$norm_state, ordered=T, levels=c("BEFORE", "AFTER"))
  print(
  ggplot(before_after_means, aes(x = sample, y = means, fill = norm_state)) +
    geom_col(position = position_dodge()) +
    facet_wrap(~ channel, ncol = col_number, scales = "free") + 
    scale_fill_manual(values = bar_fills) +
    theme_cowplot() +
    theme(text=element_text(size=14),
                            axis.text.x = element_text(color = "black", size = 8,
                                                       angle = 45, hjust = 1, vjust = 1, face = "plain"),
                            axis.text.y = element_text(color = "black", size = 10,
                                                       angle = 0, hjust = .5, vjust = 0.5, face = "plain"),
                            axis.title.x = element_blank(),
                            axis.title.y = element_text(margin=margin(t = 0, r = 80, b = 0, l = 0, unit = "pt")),
                            legend.margin = margin(t = 0, r = 10, b = 0, l = 10, unit = "pt"),
                            plot.margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")
    ) +
    labs(y = "Mean signal intensity", fill = "Norm. state")
  )
  ggsave(paste0(date, "_", project_name, "_channel_means_before_after_per_anchor.pdf"), 
         plot = last_plot(), device = "pdf", path = out_norm_aid_diag_folder, 
         scale = 0.7, width = 0.4*col_number*length(unique(before_after_means$sample)), height = 5*ceiling(length(unique(before_after_means$channel))/col_number))
}