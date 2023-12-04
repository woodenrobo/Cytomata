
sample_size_bars <- function() {
  count_table <- as.data.frame(table(factor(exprs_set$sample, levels = stringr::str_sort(unique(sample), numeric = TRUE))))
  setwd(output_data_sub_analysis)
  write.csv(x = count_table, file = paste0(data_sub, "_sample_count_table.csv"))

  #backup <- exprs_set
  if (sampling_rate > 1) {
    barcol_palette <- c("no" = "darkorange", "yes" = "darkred")
  }


  #exprs_set <- exprs_set[exprs_set$sample %in% unique(exprs_set$sample)[1:10], ]

  pdf(file = paste0(output_data_sub_analysis, "events_per_sample_", date, ".pdf"),
      width = 4,
      height = 2 + (length(unique(exprs_set$sample)) * 0.07))
  print(ggplot(exprs_set,
    aes(x = factor(exprs_set$sample, levels = stringr::str_sort(unique(sample), numeric = TRUE)))) +
    geom_bar(aes(fill = resampled)) +
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
  palette <- Polychrome::createPalette(length(unique(exprs_set[, variable_name])), c("#010101", "#ff0000"), M = 10000)
  names(palette) <- unique(exprs_set[, variable_name])
}