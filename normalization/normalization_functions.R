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
            temp2 <- c(a_sample, channel, sum(abs(a - b)))
            anchor_differences <- rbind(anchor_differences, temp2)

        }
    }
    colnames(anchor_differences) <- c("sample", "channel", "abs_difference")

    setwd(out_norm_tables_folder)
    write.csv(anchor_differences, file = "anchor_differences_local.csv")

    return(anchor_differences)
}
    
global_anchor_difference_compute <- function() {
    #computing most "average" anchor sample throughout all channels
    anchor_differences_global <- as.data.frame(anchor_differences %>%
                                    group_by(sample) %>%
                                    summarize(sum_difference = sum(as.numeric(abs_difference))))

    setwd(out_norm_tables_folder)
    write.csv(anchor_differences_global, file = "anchor_differences_global.csv")

    return(anchor_differences_global)
}

ks_diss_compute <- function() {
    #computing most "average" anchor sample via Kolmogorov-Smirnov dissimilarity metric
    #per channel
    ks_diss <- data.frame()
    for (channel in feature_markers){
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
        cat("Calculating", channel, "channel\n")
        for (a_sample in unique(quantiles_table_long$a_sample)) {
            temp_diss <- c()
            for (b_sample in unique(quantiles_table_long$a_sample)[!unique(quantiles_table_long$a_sample) %in% a_sample]) {
                temp_diss <- c(temp_diss, KS.diss(exprs_set[exprs_set$sample == a_sample, channel], exprs_set[exprs_set$sample == b_sample, channel]))
            }
            ks_diss <- rbind(ks_diss, c(a_sample, channel, sum(temp_diss)))
        }
        pb$tick()
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


    #plot density ridges with optimal anchor and percentile highlighted
    pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(feature_markers),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = FALSE,    # If TRUE, clears the bar when finish
                    width = 100)      # Width of the progress bar
    
    percentile_selector <- c()
    for (channel in feature_markers){
        temp_quantiles <- quantiles_table_long[quantiles_table_long$channel == channel, ]
        temp_local_optimal_anchor <- optimal_anchor[optimal_anchor$channel == channel, ]

        scaling_factors <- cbind(temp_quantiles[, c(1:2)], 
                            apply(temp_quantiles[, -c(1:2)], MARGIN = 2, FUN = function(x) as.numeric(x[temp_quantiles$a_sample == temp_local_optimal_anchor$best_anchor]) / as.numeric(x)))

        temp_set <- exprs_set

        #temp_set[, colnames(temp_set) %in% feature_markers] <- apply(temp_set[, colnames(temp_set) %in% feature_markers], MARGIN = 2, FUN = function(x) sinh(x) * cofac)

        baseline_var <- temp_set %>% pull(channel)  %>% var()

        baseline_means_var <- temp_set %>% group_by(sample) %>% summarise(means = mean(!!sym(channel))) %>% summarize(var = var(means))

        temp_x <- temp_set %>%
                    filter(sample == temp_local_optimal_anchor$best_anchor) %>%
                    pull(channel)

        temp_y <- temp_set %>%
                    filter(sample != temp_local_optimal_anchor$best_anchor) %>%
                    pull(channel)
        
        baseline_ks <- KS.diss(temp_x, temp_y)


        for (percentile in colnames(scaling_factors[-c(1:2)])) {
            if (sum(is.na(scaling_factors[, percentile])) > 0 | sum(is.infinite(scaling_factors[, percentile])) > 0 | sum(scaling_factors[, percentile] == 0) > 0) { # nolint
                cat(paste0(channel, ' ', percentile, " scaling factors contain 0, NA or Inf values!\nSkipping.\n"))
            } else {
                for (samp in unique(temp_set$sample)) {
                    temp_set[temp_set$sample == samp, channel] <- temp_set[temp_set$sample == samp, channel] * as.numeric(scaling_factors[scaling_factors$a_sample == samp, percentile])
                }

                temp_x <- temp_set %>%
                            filter(sample == temp_local_optimal_anchor$best_anchor) %>%
                            pull(channel)

                temp_y <- temp_set %>%
                            filter(sample != temp_local_optimal_anchor$best_anchor) %>%
                            pull(channel)
                
                temp_ks <- KS.diss(temp_x, temp_y)

                temp_var <- temp_set %>% pull(channel) %>% var()

                temp_means_var <- temp_set %>% group_by(sample) %>% summarise(means = mean(!!sym(channel))) %>% summarize(var = var(means))

                percentile_selector <- rbind(percentile_selector, c(channel, percentile, baseline_ks, temp_ks, temp_ks - baseline_ks, baseline_var, temp_var, temp_var - baseline_var, baseline_means_var, temp_means_var, temp_means_var - baseline_means_var))
            }
        }
        pb$tick()
    }
    percentile_selector <- data.frame(percentile_selector)
    colnames(percentile_selector) <- c("channel", "percentile", "baseline_ks", "result_ks", "reduction_in_ks", "baseline_var", "result_var", "reduction_in_var", "baseline_means_var", "result_means_var", "reduction_in_means_var")

    percentile_selector$percentile_optimality <- rescale(as.numeric(percentile_selector$reduction_in_ks)) + rescale(as.numeric(percentile_selector$reduction_in_var)) + rescale(as.numeric(percentile_selector$reduction_in_means_var))


    setwd(out_norm_tables_folder)
    write.csv(percentile_selector, file = "percentile_selector_table.csv")

    return(percentile_selector)
}



optimal_percentile_compute <- function() {

    optimal_percentile <- c()
    for (channel in feature_markers){
        temp_selector <- percentile_selector[percentile_selector$channel == channel, ]
        optimal_percentile <- rbind(optimal_percentile, c(channel, temp_selector$percentile[which.min(temp_selector$percentile_optimality)]))
    }
    optimal_percentile <- data.frame(optimal_percentile)
    colnames(optimal_percentile) <- c("channel", "percentile")


    setwd(out_norm_tables_folder)
    write.csv(optimal_percentile, file = "optimal_percentiles_table.csv")

    return(optimal_percentile)
}



