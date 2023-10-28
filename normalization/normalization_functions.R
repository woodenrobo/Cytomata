quantile_table <- function() {

    quantiles_table_long <- data.frame()
    #creates a table with quantile values for each channel and anchor sample
    for (channel in feature_markers){
        for (a_sample in unique(exprs_set$sample)){
            p60 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.6)
            p70 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.7)
            p75 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.75)
            p80 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.8)
            p85 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.85)
            p90 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.90)
            p95 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.95)
            p97 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.97)
            p99 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.99)
            p99_5 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.995)
            p99_9 <- quantile(exprs_set[exprs_set$sample==a_sample, channel], 0.999)

            #save a table with quantile values
            quantiles_table_long <- rbind(quantiles_table_long, c(channel, a_sample, p60, p70, p75, p80, p85, p90, p95, p97, p99, p99_5, p99_9))
            colnames(quantiles_table_long) <- c("channel", "a_sample", "p60", "p70", "p75", "p80", "p85", "p90", "p95", "p97", "p99", "p99_5", "p99_9")
        }
    }
    setwd(out_norm_tables_folder)
    write.csv(quantiles_table_long, file = "quantiles_per_channel_sample.csv")

    return(quantiles_table_long)
}

quant_variation_table <- function() {
    
    quantiles_table_short <- data.frame()
    for (channel in feature_markers){
        #save a table with variation for each channel and quantile
        var_p60 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p60"])
        var_p70 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p70"])
        var_p75 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p75"])
        var_p80 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p80"])
        var_p85 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p85"])
        var_p90 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p90"])
        var_p95 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p95"])
        var_p97 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p97"])
        var_p99 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p99"])
        var_p99_5 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p99_5"])
        var_p99_9 <- var(quantiles_table_long[quantiles_table_long$channel==channel, "p99_9"])
        quantiles_table_short <- rbind(quantiles_table_short, c(channel, var_p60, var_p70, var_p75, var_p80, var_p85, var_p90, var_p95, var_p97, var_p99, var_p99_5, var_p99_9))
        colnames(quantiles_table_short) <- c("channel", "p60", "p70", "p75", "p80", "p85", "p90", "p95", "p97", "p99", "p99_5", "p99_9")
    }
    setwd(out_norm_tables_folder)
    write.csv(quantiles_table_short, file = "quantile_variation_per_channel.csv")

    return(quantiles_table_short)
}

anchor_difference_compute <- function() {
    

    #computing most "average" anchor sample via quantile values
    #per channel
    anchor_differences <- data.frame()
    for (channel in feature_markers){
        for (a_sample in unique(quantiles_table_long$a_sample)) {
            temp <- quantiles_table_long[quantiles_table_long$channel == channel, ]
            a <- mutate_all(temp[, -c(1:2)], function(x) as.numeric(as.character(x)))
            b <- mutate_all(temp[temp$a_sample == a_sample, -c(1:2), ][rep(1, nrow(temp[, -c(1:2)])), ], function(x) as.numeric(as.character(x)))
            temp2 <- c(a_sample, channel, mean(as.numeric(unlist(abs(a - b)))))
            anchor_differences <- rbind(anchor_differences, temp2)

        }
    }
    colnames(anchor_differences) <- c("sample", "channel", "mean_abs_difference")

    setwd(out_norm_tables_folder)
    write.csv(anchor_differences, file = "anchor_differences_local.csv")

    return(anchor_differences)
}

global_anchor_difference_compute <- function() {
    #computing most "average" anchor sample throughout all channels
    anchor_differences_global <- as.data.frame(anchor_differences %>%
                                    group_by(sample) %>%
                                    summarize(mean_abs_difference = mean(as.numeric(mean_abs_difference))))

    setwd(out_norm_tables_folder)
    write.csv(anchor_differences_global, file = "anchor_differences_global.csv")

    return(anchor_differences_global)
}

ks_diss_compute <- function() {
    cat("Calcualting Kolmogorov-Smirnov dissimilarity\nEach anchor`s channel vs total channel events\n")
    #computing most "average" anchor sample via Kolmogorov-Smirnov dissimilarity metric
    #per channel, for each sample vs all others
    pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(feature_markers),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = FALSE,    # If TRUE, clears the bar when finish
                    width = 100)      # Width of the progress bar


    ks_diss <- data.frame()
    for (channel in feature_markers){
        pb$tick()
        for (a_sample in unique(quantiles_table_long$a_sample)) {
            ks_diss <- rbind(ks_diss, c(a_sample, channel, KS.diss(exprs_set[exprs_set$sample == a_sample, channel], exprs_set[!exprs_set$sample == a_sample, channel])))
        }
    }
    colnames(ks_diss) <- c("sample", "channel", "ks_diss")
    
    setwd(out_norm_tables_folder)
    write.csv(ks_diss, file = "ks_dissim_local.csv")

    return(ks_diss)
}

ks_diss_pairwise_compute <- function() {
    cat("Calcualting pairwise Kolmogorov-Smirnov dissimilarity\n")
    #computing most "average" anchor sample via Kolmogorov-Smirnov dissimilarity metric
    #per channel, PAIRWISE for each sample
    pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(feature_markers),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = FALSE,    # If TRUE, clears the bar when finish
                    width = 100)      # Width of the progress bar
    

    ks_diss <- data.frame()
    for (channel in feature_markers){
        pb$tick()
        cat("Calculating", channel, "channel\n")
        for (a_sample in unique(quantiles_table_long$a_sample)) {
            temp_diss <- c()
            for (b_sample in unique(quantiles_table_long$a_sample)[!unique(quantiles_table_long$a_sample) %in% a_sample]) {
                temp_diss <- c(temp_diss, KS.diss(exprs_set[exprs_set$sample == a_sample, channel], exprs_set[exprs_set$sample == b_sample, channel]))
            }
            ks_diss <- rbind(ks_diss, c(a_sample, channel, mean(temp_diss)))
        }
        
    }
    colnames(ks_diss) <- c("sample", "channel", "ks_diss")
    
    setwd(out_norm_tables_folder)
    write.csv(ks_diss, file = "ks_dissim_local_pairwise.csv")

    return(ks_diss)
}

ks_diss_global_compute <- function() {
    #computing most "average" anchor sample throughout all channels
    ks_diss_global <- as.data.frame(ks_diss %>%
                                    group_by(sample) %>%
                                    summarize(sum_difference = sum(as.numeric(ks_diss))))

    setwd(out_norm_tables_folder)
    write.csv(ks_diss_global, file = "ks_dissim_global.csv")
        
    return(ks_diss_global)
}

percentile_selector_compute <- function() {

    cat("Testing percentiles\n")
    #plot density ridges with optimal anchor and percentile highlighted
    pb <- progress_bar$new(format = "Testing percentiles\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(feature_markers),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = FALSE,    # If TRUE, clears the bar when finish
                    width = 125)      # Width of the progress bar
    
    
    temp_set_norm <- exprs_set

    percentile_selector <- c()
    for (channel in feature_markers){
        pb$tick()

        baseline_quantiles <- quantiles_table_long[quantiles_table_long$channel == channel, ]
        temp_local_optimal_anchor <- optimal_anchor[optimal_anchor$channel == channel, ]

        scaling_factors <- cbind(baseline_quantiles[, c(1:2)], 
                            apply(baseline_quantiles[, -c(1:2)], MARGIN = 2, FUN = function(x) as.numeric(x[baseline_quantiles$a_sample == temp_local_optimal_anchor$best_anchor]) / as.numeric(x)))


        #exprs_set[, colnames(exprs_set) %in% feature_markers] <- apply(exprs_set[, colnames(exprs_set) %in% feature_markers], MARGIN = 2, FUN = function(x) sinh(x) * cofac)

        baseline_var <- exprs_set %>% pull(channel)  %>% var()

        baseline_means_var <- exprs_set %>% group_by(sample) %>% summarise(means = mean(!!sym(channel))) %>% summarize(var = var(means))

        if (ks_testing == "total") {
            temp_x <- exprs_set %>%
                        filter(sample == temp_local_optimal_anchor$best_anchor) %>%
                        pull(channel)

            temp_y <- exprs_set %>%
                        filter(sample != temp_local_optimal_anchor$best_anchor) %>%
                        pull(channel)
            
            baseline_ks <- KS.diss(temp_x, temp_y)
        }
        if (ks_testing == "pairwise") {
            temp_diss <- c()
            for (a_sample in unique(exprs_set$sample)[!unique(exprs_set$sample) %in% a_sample]) {
                temp_diss <- c(temp_diss, KS.diss(exprs_set[exprs_set$sample == temp_local_optimal_anchor$best_anchor, channel], exprs_set[exprs_set$sample == a_sample, channel]))
            }
            baseline_ks <- mean(temp_diss)
        }
        
       

        #computing total distance between all quantiles
        baseline_quantile_distance <- mutate_all(baseline_quantiles[, -c(1:2)], function(x) as.numeric(as.character(x)))
        baseline_quantile_distance <- sum(apply(baseline_quantile_distance, MARGIN = 2, FUN = function(x) stats::dist(x)))


        for (percentile in colnames(scaling_factors[-c(1:2)])) {
            if (sum(is.na(scaling_factors[, percentile])) > 0 | sum(is.infinite(scaling_factors[, percentile])) > 0 | sum(scaling_factors[, percentile] == 0) > 0) { # nolint
                cat(paste0(channel, ' ', percentile, " scaling factors contain 0, NA or Inf values!\nSkipping.\n"))
            } else {
                for (samp in unique(temp_set_norm$sample)) {
                    temp_set_norm[temp_set_norm$sample == samp, channel] <- exprs_set[exprs_set$sample == samp, channel] * as.numeric(scaling_factors[scaling_factors$a_sample == samp, percentile])
                }
                if (ks_testing == "total") {
                    temp_x <- temp_set_norm %>%
                                filter(sample == temp_local_optimal_anchor$best_anchor) %>%
                                pull(channel)

                    temp_y <- temp_set_norm %>%
                                filter(sample != temp_local_optimal_anchor$best_anchor) %>%
                                pull(channel)
                    
                    temp_ks <- KS.diss(temp_x, temp_y)
                }
                if (ks_testing == "pairwise") {
                    temp_diss <- c()
                    for (a_sample in unique(temp_set_norm$sample)[!unique(temp_set_norm$sample) %in% a_sample]) {
                        temp_diss <- c(temp_diss, KS.diss(temp_set_norm[temp_set_norm$sample == temp_local_optimal_anchor$best_anchor, channel], temp_set_norm[temp_set_norm$sample == a_sample, channel]))
                    }
                    temp_ks <- mean(temp_diss)
                    
                }

                temp_var <- temp_set_norm %>% pull(channel) %>% var()

                temp_means_var <- temp_set_norm %>% group_by(sample) %>% summarise(means = mean(!!sym(channel))) %>% summarize(var = var(means))

                temp_quantiles <- data.frame()
                 for (samp in unique(temp_set_norm$sample)){
                    p60 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.6)
                    p70 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.7)
                    p75 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.75)
                    p80 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.8)
                    p85 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.85)
                    p90 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.90)
                    p95 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.95)
                    p97 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.97)
                    p99 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.99)
                    p99_5 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.995)
                    p99_9 <- quantile(temp_set_norm[temp_set_norm$sample==samp, channel], 0.999)

                    #save a table with quantile values
                    temp_quantiles <- rbind(temp_quantiles, c(channel, samp, p60, p70, p75, p80, p85, p90, p95, p97, p99, p99_5, p99_9))
                    colnames(temp_quantiles) <- c("channel", "samp", "p60", "p70", "p75", "p80", "p85", "p90", "p95", "p97", "p99", "p99_5", "p99_9")
                }


                temp_quantiles <- mutate_all(temp_quantiles[, -c(1:2)], function(x) as.numeric(as.character(x)))
                temp_quantile_distance <- sum(apply(temp_quantiles, MARGIN = 2, FUN = function(x) stats::dist(x)))


                percentile_selector <- rbind(percentile_selector, c(channel, percentile,
                                         baseline_ks, temp_ks, temp_ks - baseline_ks, 
                                         baseline_var, temp_var, temp_var - baseline_var, 
                                         baseline_means_var, temp_means_var, temp_means_var - baseline_means_var,
                                         baseline_quantile_distance, temp_quantile_distance, temp_quantile_distance - baseline_quantile_distance
                                         ))
            }
        }
    }
    
    percentile_selector <- data.frame(percentile_selector)
    colnames(percentile_selector) <- c("channel", "percentile",
                                     "baseline_ks", "result_ks", "reduction_in_ks",
                                      "baseline_var", "result_var", "reduction_in_var",
                                       "baseline_means_var", "result_means_var", "reduction_in_means_var",
                                       "baseline_quantile_distance", "result_quantile_distance", "reduction_in_quantile_distance")

    percentile_selector$percentile_optimality <- (rescale(as.numeric(percentile_selector$reduction_in_ks)) * 1.5 +
                                                     rescale(as.numeric(percentile_selector$reduction_in_var)) * 1.5 +
                                                      rescale(as.numeric(percentile_selector$reduction_in_means_var)) * 2.5 +
                                                      rescale(as.numeric(percentile_selector$reduction_in_quantile_distance))) * -1
                                                      


    setwd(out_norm_tables_folder)
    write.csv(apply(percentile_selector, 2, as.character), file = "percentile_selector_table.csv")
    
    return(percentile_selector)
}

optimal_percentile_compute <- function() {

    optimal_percentile <- c()
    for (channel in feature_markers){
        temp_selector <- percentile_selector[percentile_selector$channel == channel, ]
        optimal_percentile <- rbind(optimal_percentile, c(channel, temp_selector$percentile[which.max(temp_selector$percentile_optimality)]))
    }
    optimal_percentile <- data.frame(optimal_percentile)
    colnames(optimal_percentile) <- c("channel", "percentile")


    setwd(out_norm_tables_folder)
    write.csv(apply(optimal_percentile,2,as.character), file = "optimal_percentiles_table.csv")

    return(optimal_percentile)
}

normalize_batches <- function() {

    cat(paste0("\n ANCHOR SAMPLE SELECTED IS: ", a_id, "\n"))
    cat(paste0("\n BATCHES TO BE NORMALIZED ARE: \n"))
    setwd(debar_folder)
    anchor_batches_in_dir <- as.character(unique(target_anchors$batch[target_anchors$fcs %in% dir()]))
    if (a_counter > 1) {
       anchor_batches_in_dir <- anchor_batches_in_dir[!grepl(pre_norm_batch, anchor_batches_in_dir)] 
    }
    print(anchor_batches_in_dir)

    files_needed <- meta$fcs[meta$batch %in% target_anchors$batch]
    total_input <- files_needed[files_needed %in% dir()]
    if (length(total_input) != length(files_needed)) {
        warning(paste0("Only ", length(total_input), " out of ", length(files_needed), " samples in meta are present in input directory"))
    }

    if (a_counter > 1) {
       total_input <- total_input[!grepl(pre_norm_batch, total_input)] 
    }
    
    
    downsampling_rate <- 1
    #settings for transformation
    asinh_transform <- FALSE
    cofac <- 1

    for (batch in anchor_batches_in_dir) {
        input <- total_input[grepl(batch, total_input)]
        cat("Normalizing batch:", batch, "\n")
        setwd(debar_folder)
        exprs_set <- inject_fcs(input, filter_features = FALSE, asinh_transform = asinh_transform, cofac = cofac)
        
        for (channel in feature_markers){
            for (samp in unique(exprs_set$sample)) {
                exprs_set[exprs_set$sample == samp, channel] <- exprs_set[exprs_set$sample == samp, channel] *
                                                                 as.numeric(filtered_factors[grepl(batch, filtered_factors$a_sample) &
                                                                                             filtered_factors$channel == channel, "factor"])
            }
        }


        #WRITING FILES
        batch_files <- unique(exprs_set[grepl(batch, exprs_set$sample), "sample"])
        
        
        # Initializes the progress bar
        pb <- progress_bar$new(format = "Writing files \n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                            total = length(batch_files),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 100)      # Width of the progress bar
        

        for (file in batch_files){
            pb$tick()
            setwd(norm_folder)
            temp_file <- exprs_set[exprs_set$sample==file, ]
            temp_file <- flowFrame(data.matrix(temp_file[,!colnames(temp_file) %in% 'sample']))
            temp_file@parameters@data$desc <- fcs_channel_desc
            temp_file@parameters@data$name <- fcs_channel_name
            flowCore::write.FCS(x=temp_file, filename=paste0(file))
        }

    }
    

}

channel_mean_compute <- function(data_set) {
    means_df <- data.frame()
    for (channel in feature_markers) {
        temp_means <- data_set %>% group_by(sample) %>% summarise(means = mean(!!sym(channel)))
        temp_means$channel <- channel
        means_df <- rbind(means_df, temp_means)
    }

    return(means_df)
}

channel_mean_dist_compute <- function() {
    means_df <- data.frame()
    means_dist_result <- data.frame()
    for (channel in feature_markers) {
        temp_means <- exprs_set %>% group_by(sample) %>% summarise(means = mean(!!sym(channel)))
        temp_means$channel <- channel
        means_df <- rbind(means_df, temp_means)
    }
    for (channel in feature_markers) {
        mean_dist <- data.frame()
        for (a_sample in unique(means_df$sample)) {
            temp_mean_dist <- c()
            for (b_sample in unique(means_df$sample)[!unique(means_df$sample) %in% a_sample]) {
                temp_mean_dist <- c(temp_mean_dist,
                means_df[means_df$sample == a_sample & means_df$channel == channel, "means"] -
                means_df[means_df$sample == b_sample & means_df$channel == channel, "means"])
            }
            mean_dist <- rbind(mean_dist, c(a_sample, channel, mean(abs(unlist(temp_mean_dist)))))
            colnames(mean_dist) <- c("sample", "channel", "mean_dist")
        }
        means_dist_result <- rbind(means_dist_result, mean_dist)

    }

    
    
    setwd(out_norm_tables_folder)
    write.csv(apply(means_dist_result, 2, as.character), file = "channel_intensity_mean_distances.csv")

    return(means_dist_result)
}
