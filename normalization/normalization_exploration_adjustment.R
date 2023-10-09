
a_batch_table <- c()
a_counter <- 0
for (a_id in anchor_ids) {

    cat(paste0("\n==========\nEXPLORATION AND SETUP MODULE STARTED\n=========="))

    a_counter <- a_counter + 1
    if (a_counter == 1) {
        out_norm_folder <- paste0(output_folder, "normalization", "/")
        ifelse(!dir.exists(out_norm_folder), dir.create(out_norm_folder), FALSE)

        out_norm_aid_folder <- paste0(out_norm_folder, a_id, "/")
        ifelse(!dir.exists(out_norm_aid_folder), dir.create(out_norm_aid_folder), FALSE)

        out_norm_tables_folder <- paste0(out_norm_aid_folder, "tables", "/")
        ifelse(!dir.exists(out_norm_tables_folder), dir.create(out_norm_tables_folder), FALSE)

        out_norm_dens_folder <- paste0(out_norm_aid_folder, "densities", "/")
        ifelse(!dir.exists(out_norm_dens_folder), dir.create(out_norm_dens_folder), FALSE)



        target_anchors <- meta[meta$id == a_id, ]
        cat(paste0("\n ANCHOR SAMPLE SELECTED IS: ", a_id, "\n"))
        cat(paste0("\n BATCHES PRESENT IN INPUT ARE: \n"))
        setwd(debar_folder)
        print(unique(target_anchors$batch[target_anchors$fcs %in% dir()]))

        temp_a_batch_table <- c()
        temp_a_batch_table$batches <- unique(target_anchors$batch[target_anchors$fcs %in% dir()])
        temp_a_batch_table$anchor_ids <- a_id
        temp_a_batch_table <- data.frame(temp_a_batch_table)
        a_batch_table <- bind_rows(a_batch_table, temp_a_batch_table)

        input <- target_anchors$fcs[target_anchors$fcs %in% dir()]
        downsampling_rate <- 1
        #settings for transformation
        asinh_transform <- FALSE
        cofac <- 1
        exprs_set <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)

        #compute quantile values for each channel and anchor sample
        quantiles_table_long <- quantile_table()
        #compute variation for each channel and quantile
        quantile_variation_table <- quant_variation_table()

        #computing most "average" anchor sample via quantile values
        #per channel
        anchor_differences <- anchor_difference_compute()
        

        # #computing most "average" anchor sample throughout all channels
        # anchor_differences_global <- global_anchor_difference_compute()

        
        #computing most "average" anchor sample via Kolmogorov-Smirnov dissimilarity metric
        #per channel
        ks_diss <- ks_diss_compute()

        # #computing most "average" anchor sample throughout all channels
        # ks_diss_global <- ks_diss_global_compute()


        # #combining distance between selected percentiles and Kolmogorov-Smirnov dissimilarity metrics
        # #to select an optimal anchor for all channels
        # global_anchor_selector <- data.frame(anchor_differences_global$sample, c(rescale(anchor_differences_global$sum_difference) + rescale(ks_diss_global$sum_difference)))
        # optimal_global_anchor <- global_anchor_selector[which.min(global_anchor_selector[, 2]), 1]
        # setwd(out_norm_tables_folder)
        # write.csv(optimal_global_anchor, file = "optimal_anchor_global.csv")

        #combining distance between selected percentiles and Kolmogorov-Smirnov dissimilarity metrics
        #to select an optimal anchor for EACH channel
        anchor_selector <- data.frame(anchor_differences$sample, anchor_differences$channel, c(rescale(as.numeric(anchor_differences$abs_difference)) + rescale(as.numeric(ks_diss$ks_diss))))
        colnames(anchor_selector) <- c("sample", "channel", "optimality")
        optimal_anchor <- anchor_selector %>%
                                    group_by(channel) %>%
                                    summarize(best_anchor = sample[which.min(optimality)])
        setwd(out_norm_tables_folder)
        write.csv(optimal_anchor, file = "optimal_anchor_local.csv")



        #IDEA: 
        #calculate scaling factors for optimal global and local anchors
        #subsample each anchor for less CPU time
        #adjust the subsampled anchors
        #compute KS.diss from optimal anchor to all others after adjustment with each percentile
        #compute percentile variation after adjustment with each percentile
        #percentile that resutls in least amount of variation in the channel after adjustment is optimal


        percentile_selector <- percentile_selector_compute()

        optimal_percentile <- optimal_percentile_compute()



        

        
        if (hide_zeroes_in_ridges == 1) {
            #optional
            #plot density ridges with optimal anchor and percentile highlighted wothout zero values (for better visibility in some channels)
            pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                            total = length(feature_markers),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 100)      # Width of the progress bar

            for (channel in feature_markers) {
                exploration_ridges_wo_zeroes()
                pb$tick()
            }
        } else {
            #plot density ridges with optimal anchor and percentile highlighted
            pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                            total = length(feature_markers),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 100)      # Width of the progress bar

            for (channel in feature_markers) {
                exploration_ridges()
                pb$tick()
            }
        }

        settings_table <- left_join(optimal_percentile, optimal_anchor)
        setwd(out_norm_aid_folder)
        write.csv(settings_table, file = "normalization_settings.csv")
        

            
        if (interactive()) {
            answer <- readline(paste0("Are you satisfied with automatic settings?\n",
                            "If not, change settings table in\n",
                            "Cytomata_data/<project_folder>/output/normalization/<anchor_id>/\n",
                            "please type yes when you are ready   "))
            } else {
            answer <- "yes"
        }
        
        if (answer == "yes") {
            cat(paste0("Continuing with settings from ", a_id, "/normalization_settings.csv"))
            setwd(out_norm_aid_folder)
            settings_table <- read.csv("normalization_settings.csv", row.names = 1)

            cat(paste0("Reading in anchors for final scaling factor computing\n"))
            setwd(debar_folder)
            input <- target_anchors$fcs[target_anchors$fcs %in% dir()]
            downsampling_rate <- 1
            #settings for transformation
            asinh_transform <- FALSE 
            cofac <- 1
            exprs_set <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)

            #compute quantile values for each channel and anchor sample
            quantiles_table_long <- quantile_table()

            cat("Computing scaling factors\n")
            setwd(out_norm_tables_folder)
            optimal_anchor <- read.csv("optimal_anchor_local.csv", row.names = 1)

            scaling_factors <- c()
            filtered_factors <- c()
            for (channel in feature_markers){
                temp_quantiles <- quantiles_table_long[quantiles_table_long$channel == channel, ]
                temp_local_optimal_anchor <- optimal_anchor[optimal_anchor$channel == channel, ]

                temp_scaling_factors <- cbind(temp_quantiles[, c(1:2)], 
                                            apply(temp_quantiles[, -c(1:2)], MARGIN = 2, FUN = function(x) as.numeric(x[temp_quantiles$a_sample == temp_local_optimal_anchor$best_anchor]) / as.numeric(x)))
                scaling_factors <- rbind(scaling_factors, temp_scaling_factors)

                temp_filtered_factors <- temp_scaling_factors
                temp_filtered_factors$percentile <- settings_table[settings_table$channel == channel, "percentile"]
                temp_filtered_factors$factor <- temp_filtered_factors[,settings_table[settings_table$channel == channel, "percentile"]]
                temp_filtered_factors <- temp_filtered_factors[, c("channel", "a_sample", "percentile", "factor")]
                filtered_factors <- rbind(filtered_factors, temp_filtered_factors)

            }
            setwd(out_norm_tables_folder)
            write.csv(scaling_factors, file = "scaling_factors.csv")

            setwd(out_norm_tables_folder)
            write.csv(filtered_factors, file = "filtered_scaling_factors.csv")

            scaling_factors_barplots()
            


        }
        

    } else {

        out_norm_folder <- paste0(output_folder, "normalization", "/")
        ifelse(!dir.exists(out_norm_folder), dir.create(out_norm_folder), FALSE)

        out_norm_aid_folder <- paste0(out_norm_folder, a_id, "/")
        ifelse(!dir.exists(out_norm_aid_folder), dir.create(out_norm_aid_folder), FALSE)

        out_norm_tables_folder <- paste0(out_norm_aid_folder, "tables", "/")
        ifelse(!dir.exists(out_norm_tables_folder), dir.create(out_norm_tables_folder), FALSE)

        out_norm_dens_folder <- paste0(out_norm_aid_folder, "densities", "/")
        ifelse(!dir.exists(out_norm_dens_folder), dir.create(out_norm_dens_folder), FALSE)



        target_anchors <- meta[meta$id == a_id, ]
        cat(paste0("\n ANCHOR SAMPLE SELECTED IS: ", a_id, "\n"))
        cat(paste0("\n BATCHES PRESENT IN INPUT ARE: \n"))
        setwd(debar_folder)
        print(unique(target_anchors$batch[target_anchors$fcs %in% dir()]))

        temp_a_batch_table <- c()
        temp_a_batch_table$batches <- unique(target_anchors$batch[target_anchors$fcs %in% dir()])
        temp_a_batch_table$anchor_ids <- a_id
        temp_a_batch_table <- data.frame(temp_a_batch_table)
        a_batch_table <- bind_rows(a_batch_table, temp_a_batch_table)
        #we have to use the anchor, normalized in the step before
        a_batch_table <- a_batch_table[duplicated(a_batch_table$batches, fromLast = FALSE) | duplicated(a_batch_table$batches, fromLast = TRUE), ]
        a_batch_table <- a_batch_table[a_batch_table$anchor_ids %in% c(anchor_ids[a_counter-1], anchor_ids[a_counter]), ]
        prev_norm_batch <- unique(a_batch_table$batches)
        optimal_anchor <- target_anchors$fcs[target_anchors$batch == prev_norm_batch & target_anchors$id == a_id]

        #NEED TO SELECT PRE-NORMALIZED OPTIMAL ANCHOR SOMEHOW
        input <- target_anchors$fcs[target_anchors$fcs %in% dir()]
        #settings for transformation
        asinh_transform <- FALSE 
        cofac <- 1
        exprs_set <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)

        #exprs_set <- inject_fcs(input, filter_features = TRUE, asinh_transform = FALSE, cofac = cofac)
        #compute quantile values for each channel and anchor sample
        quantiles_table_long <- quantile_table()
        #compute variation for each channel and quantile
        quantile_variation_table <- quant_variation_table()


        #IDEA: 
        #calculate scaling factors for optimal global and local anchors
        #subsample each anchor for less CPU time
        #adjust the subsampled anchors
        #compute KS.diss from optimal anchor to all others after adjustment with each percentile
        #compute percentile variation after adjustment with each percentile
        #percentile that resutls in least amount of variation in the channel after adjustment is optimal


        percentile_selector <- percentile_selector_compute()

        optimal_percentile <- optimal_percentile_compute()



        

        
        if (hide_zeroes_in_ridges == 1) {
            #optional
            #plot density ridges with optimal anchor and percentile highlighted wothout zero values (for better visibility in some channels)
            pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                            total = length(feature_markers),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 100)      # Width of the progress bar

            for (channel in feature_markers) {
                exploration_ridges_wo_zeroes
                pb$tick()
            }
        } else {
            #plot density ridges with optimal anchor and percentile highlighted
            pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                            total = length(feature_markers),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 100)      # Width of the progress bar

            for (channel in feature_markers) {
                exploration_ridges()
                pb$tick()
            }
        }

        settings_table <- left_join(optimal_percentile, optimal_anchor)
        setwd(out_norm_aid_folder)
        write.csv(settings_table, file = "normalization_settings.csv")
        

    }
    
}

# this code detects peaks, dont know what to do with it yet, cause sometimes the peaks are more like a plateu
#   temptemp <- asinh(exprs_set$CD21[exprs_set$sample == "201208_Blut_Panel1_CV19_BC_1.fcs"] / cofac)
#   d <- density(temptemp)
#   d$x[c(F, diff(sign(diff(d$y))))<0]