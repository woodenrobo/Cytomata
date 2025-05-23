cat(paste0("\n==========\nEXPLORATION AND SETUP MODULE STARTED\n=========="))

if (norm_mode == "harmony") {


    out_norm_folder <- paste0(output_folder, "normalization", "/")
    ifelse(!dir.exists(out_norm_folder), dir.create(out_norm_folder), FALSE)

    out_norm_dens_folder <- paste0(out_norm_folder, "densities", "/")
    ifelse(!dir.exists(out_norm_dens_folder), dir.create(out_norm_dens_folder), FALSE)


    cat(paste0("Continuing with harmony algorithm\n"))

    
    cat(paste0("\n BATCHES PRESENT IN INPUT ARE: \n"))

    setwd(debar_folder)

    print(unique(meta$batch[meta$fcs %in% dir()]))

    files_needed <- meta$fcs[meta$fcs %in% dir()]

    if (length(files_needed) == 0) {
        stop("No files in input directory")
    }

    total_input <- files_needed[files_needed %in% dir()]

    if (length(total_input) != length(files_needed)) {
        warning(paste0("Only ", length(total_input), " out of ", length(files_needed), " samples in meta are present in input directory"))
    }


    sampling_rate <- 1
    #settings for transformation
    asinh_transform <- FALSE
    cofac <- 1

    setwd(debar_folder)
    exprs_set <- inject_fcs(total_input, filter_features = FALSE, asinh_transform = asinh_transform, cofac = cofac)


    if (hide_zeroes_in_ridges == 1) {
        #optional
        #plot density ridges with optimal anchor and percentile highlighted wothout zero values (for better visibility in some channels)
        pb <- progress_bar$new(format = "Plotting Estimated Densities\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                        total = length(feature_markers),
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = FALSE,    # If TRUE, clears the bar when finish
                        width = 130)      # Width of the progress bar

        for (channel in feature_markers) {
            pb$tick()
            # exploration_ridges_wo_zeroes()
            exploration_ridges_harmony_wo_zero(exprs_set, channel_name)
        }
    }

    #plot density ridges with optimal anchor and percentile highlighted
    pb <- progress_bar$new(format = "Plotting Estimated Densities\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(feature_markers),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = FALSE,    # If TRUE, clears the bar when finish
                    width = 130)      # Width of the progress bar

    for (channel in feature_markers) {
        pb$tick()
        exploration_ridges_harmony(exprs_set, channel_name)
    }


    normalize_batches_harmony()





} else if (norm_mode == "quantile") {

    cat(paste0("Continuing with quantile algorithm\n"))



    a_batch_table <- c()
    a_counter <- 0
    for (a_id in anchor_ids) {

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
            sampling_rate <- 1
            #settings for transformation
            asinh_transform <- FALSE
            cofac <- 1
            exprs_set <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)

            # compute average quantiles over all anchor samples for each channel using X quantiles
            method <- "channel" # "sample" or "channel"

            
            if (method == "sample") {

                n_quantiles <- 50000

                # compute quantile values for each channel and anchor sample
                # and average them out
                quantiles_table_long <- c()
                pb <- progress_bar$new(format = "Computing quantiles per sample\nThis is very memory hungry and can crash\n
                (:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(feature_markers) * length(unique(exprs_set$sample)),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = TRUE,    # If TRUE, clears the bar when finish
                    width = 160)      # Width of the progress bar
                for (channel in feature_markers) {
                    # computing per sample
                    for (a_sample in unique(exprs_set$sample)) {
                        temp_quantiles <- quantile(exprs_set[exprs_set$sample == a_sample, channel], probs = seq(0, 1, length.out = n_quantiles), na.rm = TRUE)
                        temp_quantiles <- data.frame(a_sample, channel, names(temp_quantiles), temp_quantiles)
                        colnames(temp_quantiles) <- c("a_sample", "channel", "quantile", "intensity")
                        quantiles_table_long <- rbind(quantiles_table_long, temp_quantiles, make.row.names = FALSE)
                        pb$tick()
                    }
                }

                # compute average quantiles for each channel using X quantiles
                target_quantiles <- quantiles_table_long %>%
                    group_by(channel, quantile) %>%
                    dplyr::summarize(intensity = mean(intensity, na.rm = TRUE)) %>%
                    ungroup() %>%
                    mutate(a_sample = "target") %>%
                    select(a_sample, channel, quantile, intensity)

                target_quantiles$quantile <- as.numeric(gsub("%", "", target_quantiles$quantile)) / 100

                # use approx function to expand the target quantiles into a target distribution for each channel
                n_points <- ifelse(n_quantiles > 100000, n_quantiles, 100000)
                rprobs <- runif(n_points, 0, 1)

                simulated_exprs_set <- c()
                simulated_exprs_set[["sample"]] <- rep("target", n_points)

                for (channel in feature_markers) {
                    temp_target_quantiles <- target_quantiles[target_quantiles$channel == channel, ]
                    temp_simulated_exprs <- approx(
                                                   as.numeric(temp_target_quantiles$quantile),
                                                   temp_target_quantiles$intensity,
                                                   xout = rprobs, rule = 2)
                    simulated_exprs_set[[channel]] <- temp_simulated_exprs$y
                }
                simulated_exprs_set[["cell_id"]] <- paste0("simulated_", seq(1, n_points))
                simulated_exprs_set <- data.frame(simulated_exprs_set)

            } else if (method == "channel") {

                n_quantiles <- 100000

                # compute quantile values for each channel over all anchor sample events
                quantiles_table_long <- c()
                pb <- progress_bar$new(format = "Computing quantiles\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(feature_markers),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = TRUE,    # If TRUE, clears the bar when finish
                    width = 120)      # Width of the progress bar
                for (channel in feature_markers) {
                    # computing per channel
                    temp_quantiles <- quantile(exprs_set[, channel], probs = seq(0, 1, length.out = n_quantiles), na.rm = TRUE)
                    temp_quantiles <- data.frame(a_sample = "all", channel, names(temp_quantiles), temp_quantiles)
                    colnames(temp_quantiles) <- c("a_sample", "channel", "quantile", "intensity")
                    quantiles_table_long <- rbind(quantiles_table_long, temp_quantiles, make.row.names = FALSE)
                    pb$tick()
                }

                quantiles_table_long$quantile <- as.numeric(gsub("%", "", quantiles_table_long$quantile)) / 100


                # use approx function to expand the target quantiles into a target distribution for each channel
                n_points <- ifelse(n_quantiles > 100000, n_quantiles, 100000)
                rprobs <- runif(n_points, 0, 1)

                simulated_exprs_set <- c()
                simulated_exprs_set[["sample"]] <- rep("target", n_points)

                for (channel in feature_markers) {
                    temp_target_quantiles <- quantiles_table_long[quantiles_table_long$channel == channel, ]
                    temp_simulated_exprs <- approx(as.numeric(temp_target_quantiles$quantile), temp_target_quantiles$intensity,
                        xout = rprobs, rule = 2)
                    simulated_exprs_set[[channel]] <- temp_simulated_exprs$y
                }
                simulated_exprs_set[["cell_id"]] <- paste0("simulated_", seq(1, n_points))
                simulated_exprs_set <- data.frame(simulated_exprs_set)

            } else {
                stop("Quantile norm method not recognized. Please use \"sample\" or \"channel\".")
            }
            


            if (hide_zeroes_in_ridges == 1) {
                #optional
                #plot density ridges with optimal anchor and percentile highlighted wothout zero values (for better visibility in some channels)
                pb <- progress_bar$new(format = "Plotting Estimated Densities\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                                total = length(feature_markers),
                                complete = "=",   # Completion bar character
                                incomplete = "-", # Incomplete bar character
                                current = ">",    # Current bar character
                                clear = FALSE,    # If TRUE, clears the bar when finish
                                width = 130)      # Width of the progress bar

                for (channel in feature_markers) {
                    pb$tick()
                    # exploration_ridges_wo_zeroes()
                    exploration_ridges_from_quantiles_wo_zero(exprs_set, simulated_exprs_set, channel)
                }
            }

            #plot density ridges with optimal anchor and percentile highlighted
            pb <- progress_bar$new(format = "Plotting Estimated Densities\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                            total = length(feature_markers),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 130)      # Width of the progress bar

            for (channel in feature_markers) {
                pb$tick()
                exploration_ridges_from_quantiles(exprs_set, simulated_exprs_set, channel)
            }
            


            #compute spline functions for each channel and batch
            mapping_func_list <- compute_quantile_mapping_functions(exprs_set = exprs_set,
                                                                    n_quantiles = n_quantiles,
                                                                    ref_quantiles = quantiles_table_long)


            #normalizing batches
            normalize_batches_quantile(mapping_func_list = mapping_func_list)


            
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
            a_batch_table_temp <- a_batch_table[duplicated(a_batch_table$batches, fromLast = FALSE) | duplicated(a_batch_table$batches, fromLast = TRUE), ]
            a_batch_table_temp <- a_batch_table_temp[a_batch_table_temp$anchor_ids %in% c(anchor_ids[a_counter-1], anchor_ids[a_counter]), ]
            pre_norm_batch <- unique(a_batch_table_temp$batches)
            pre_norm_anchor <- target_anchors$fcs[target_anchors$batch == pre_norm_batch & target_anchors$id == a_id]

            #SELECTING PRE-NORMALIZED OPTIMAL ANCHOR
            setwd(norm_folder)
            anchor_file <- dir()[grepl(pre_norm_anchor, dir())]
            input <- target_anchors$fcs[target_anchors$fcs == anchor_file]

            if (length(input) == 1) {
                #settings for transformation
                asinh_transform <- FALSE
                cofac <- 1
                exprs_set_prenorm <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
            } else if (length(input) > 1) {
                stop("Multiple pre-normalized anchors found! This is currently unsupported.\n
                Please check the metafile and remove duplicates.\n")
            } else {
                stop("PRE-NORMALIZED ANCHOR NOT FOUND! Did you run first anchor? Check metafile!\n")
            }

            setwd(debar_folder)
            input <- target_anchors$fcs[target_anchors$fcs %in% dir()]
            #remove non-pre-norm anchor
            input <- input[!input %in% pre_norm_anchor]
            #settings for transformation
            asinh_transform <- FALSE
            cofac <- 1
            exprs_set <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
            
            exprs_set <- rbind(exprs_set_prenorm, exprs_set)


            if (method == "sample") {
                print("Method sample is not supported for the second anchor for now")

                method <- "channel"
            }

            if (method == "channel") {

                n_quantiles <- 100000

                # compute quantile values for each channel over all anchor sample events
                quantiles_table_long <- c()
                pb <- progress_bar$new(format = "Computing quantiles\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(feature_markers),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = TRUE,    # If TRUE, clears the bar when finish
                    width = 120)      # Width of the progress bar
                for (channel in feature_markers) {
                    # computing per channel
                    temp_quantiles <- quantile(exprs_set_prenorm[, channel], probs = seq(0, 1, length.out = n_quantiles), na.rm = TRUE)
                    temp_quantiles <- data.frame(a_sample = "all", channel, names(temp_quantiles), temp_quantiles)
                    colnames(temp_quantiles) <- c("a_sample", "channel", "quantile", "intensity")
                    quantiles_table_long <- rbind(quantiles_table_long, temp_quantiles, make.row.names = FALSE)
                    pb$tick()
                }

                quantiles_table_long$quantile <- as.numeric(gsub("%", "", quantiles_table_long$quantile)) / 100


                # use approx function to expand the target quantiles into a target distribution for each channel
                n_points <- ifelse(n_quantiles > 100000, n_quantiles, 100000)
                rprobs <- runif(n_points, 0, 1)

                simulated_exprs_set <- c()
                simulated_exprs_set[["sample"]] <- rep(pre_norm_anchor, n_points)

                for (channel in feature_markers) {
                    temp_target_quantiles <- quantiles_table_long[quantiles_table_long$channel == channel, ]
                    temp_simulated_exprs <- approx(as.numeric(temp_target_quantiles$quantile), temp_target_quantiles$intensity,
                        xout = rprobs, rule = 2)
                    simulated_exprs_set[[channel]] <- temp_simulated_exprs$y
                }
                simulated_exprs_set[["cell_id"]] <- paste0("simulated_", seq(1, n_points))
                simulated_exprs_set <- data.frame(simulated_exprs_set)

            } else {
                stop("Quantile norm method not recognized. Please use \"sample\" or \"channel\".")
            }


            

            if (hide_zeroes_in_ridges == 1) {
                #optional
                #plot density ridges with optimal anchor and percentile highlighted wothout zero values (for better visibility in some channels)
                pb <- progress_bar$new(format = "Plotting Estimated Densities\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                                total = length(feature_markers),
                                complete = "=",   # Completion bar character
                                incomplete = "-", # Incomplete bar character
                                current = ">",    # Current bar character
                                clear = FALSE,    # If TRUE, clears the bar when finish
                                width = 130)      # Width of the progress bar

                for (channel in feature_markers) {
                    pb$tick()
                    # exploration_ridges_wo_zeroes()
                    exploration_ridges_from_quantiles_wo_zero(exprs_set, simulated_exprs_set, channel)
                }
            }

            #plot density ridges with optimal anchor and percentile highlighted
            pb <- progress_bar$new(format = "Plotting Estimated Densities\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                            total = length(feature_markers),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 130)      # Width of the progress bar

            for (channel in feature_markers) {
                pb$tick()
                exploration_ridges_from_quantiles(exprs_set, simulated_exprs_set, channel)
            }
            


            #compute spline functions for each channel and batch
            mapping_func_list <- compute_quantile_mapping_functions(exprs_set = exprs_set,
                                                                    n_quantiles = n_quantiles,
                                                                    ref_quantiles = quantiles_table_long)


            #normalizing batches
            normalize_batches_quantile(mapping_func_list = mapping_func_list)

        }
    }

} else if (norm_mode == "percentile") {

    cat(paste0("Continuing with percentile algorithm\n"))


    a_batch_table <- c()
    a_counter <- 0
    for (a_id in anchor_ids) {

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
            sampling_rate <- 1
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
            if (ks_testing == "total") {
                ks_diss <- ks_diss_compute()
            }
            if (ks_testing == "pairwise") {
                ks_diss <- ks_diss_pairwise_compute()
            }
            
            channel_mean_dist <- channel_mean_dist_compute()
            
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
            anchor_selector <- data.frame(anchor_differences$sample,
                                    anchor_differences$channel,
                                    c(rescale(as.numeric(anchor_differences$mean_abs_difference)) +
                                    rescale(as.numeric(ks_diss$ks_diss)) * 1.2 +
                                    rescale(as.numeric(channel_mean_dist$mean_dist)) * 1.2) * -1)
            colnames(anchor_selector) <- c("sample", "channel", "optimality")
            setwd(out_norm_tables_folder)
            write.csv(anchor_selector, file = "anchor_selector_local.csv")

            optimal_anchor <- anchor_selector %>%
                                        group_by(channel) %>%
                                        dplyr::summarize(best_anchor = sample[which.max(optimality)])
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
                    pb$tick()
                    exploration_ridges_wo_zeroes()
                }
            }

            #plot density ridges with optimal anchor and percentile highlighted
            pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                            total = length(feature_markers),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 100)      # Width of the progress bar

            for (channel in feature_markers) {
                pb$tick()
                exploration_ridges()
            }
            
            optimal_percentile$channel <- as.character(optimal_percentile$channel)
            settings_table <- left_join(optimal_percentile, optimal_anchor)
            setwd(out_norm_aid_folder)
            write.csv(apply(settings_table, 2, as.character), file = "normalization_settings.csv")
            

                
            if (interactive()) {
                answer <- readline(paste0("Are you satisfied with automatic settings?\n",
                                "If not, change settings table in\n",
                                "Cytomata_data/<project_folder>/output/normalization/<anchor_id>/\n",
                                "please type \"continue\" when you are ready\n
                                If you want to skip normalization for a given channel, set percentile to 0\n"))
                } else {
                answer <- "continue"
            }

            if (answer != "continue") {
                cat("It seems that your typing was inprecise, we will select \"continue\" for you")
                answer <- "continue"
            }
            
            if (answer == "continue") {
            cat(paste0("Continuing with settings from ", a_id, "/normalization_settings.csv\n"))
            setwd(out_norm_aid_folder)
            settings_table <- read.csv("normalization_settings.csv", row.names = 1)

            cat(paste0("Reading in anchors for final scaling factor computing\n"))
            setwd(debar_folder)
            input <- target_anchors$fcs[target_anchors$fcs %in% dir()]
            sampling_rate <- 1
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

                #if percentile is 0, set factor to 1 and skip adjusting the channel
                if (settings_table[settings_table$channel == channel, "percentile"] == 0) {
                    temp_filtered_factors$factor <- 1
                } else {
                    temp_filtered_factors$factor <- temp_filtered_factors[,settings_table[settings_table$channel == channel, "percentile"]]
                }
                temp_filtered_factors <- temp_filtered_factors[, c("channel", "a_sample", "percentile", "factor")]
                filtered_factors <- rbind(filtered_factors, temp_filtered_factors)

            }
            setwd(out_norm_tables_folder)
            write.csv(scaling_factors, file = "scaling_factors.csv")

            setwd(out_norm_tables_folder)
            write.csv(filtered_factors, file = "filtered_scaling_factors.csv")

            scaling_factors_barplots()
            

            #normalizing batches
            
            normalize_batches()


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
            a_batch_table_temp <- a_batch_table[duplicated(a_batch_table$batches, fromLast = FALSE) | duplicated(a_batch_table$batches, fromLast = TRUE), ]
            a_batch_table_temp <- a_batch_table_temp[a_batch_table_temp$anchor_ids %in% c(anchor_ids[a_counter-1], anchor_ids[a_counter]), ]
            pre_norm_batch <- unique(a_batch_table_temp$batches)
            pre_norm_anchor <- target_anchors$fcs[target_anchors$batch == pre_norm_batch & target_anchors$id == a_id]

            #SELECTING PRE-NORMALIZED OPTIMAL ANCHOR
            setwd(norm_folder)
            anchor_file <- dir()[grepl(pre_norm_anchor, dir())]
            input <- target_anchors$fcs[target_anchors$fcs == anchor_file]
            if (length(input) == 1){
                #settings for transformation
                asinh_transform <- FALSE
                cofac <- 1
                exprs_set_prenorm <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
            } else {
                stop("PRE-NORMALIZED ANCHOR NOT FOUND! Did you run first anchor? Check metafile!\n")
            }

            setwd(debar_folder)
            input <- target_anchors$fcs[target_anchors$fcs %in% dir()]
            #remove non-pre-norm anchor
            input <- input[!input %in% pre_norm_anchor]
            #settings for transformation
            asinh_transform <- FALSE
            cofac <- 1
            exprs_set <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
            
            exprs_set <- rbind(exprs_set_prenorm, exprs_set)
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
            optimal_anchor <- data.frame(feature_markers, rep(anchor_file, length(feature_markers)))
            colnames(optimal_anchor) <- c("channel", "best_anchor")
            setwd(out_norm_tables_folder)
            write.csv(optimal_anchor, file = "optimal_anchor_local.csv")

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
                    pb$tick()
                    exploration_ridges_wo_zeroes()
                    
                }
            }

            #plot density ridges with optimal anchor and percentile highlighted
            pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                            total = length(feature_markers),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 100)      # Width of the progress bar

            for (channel in feature_markers) {
                pb$tick()
                exploration_ridges()
                
            }
            
            
            settings_table <- left_join(optimal_percentile %>% mutate(channel = as.character(channel)), optimal_anchor %>% mutate(channel = as.character(channel)))
            setwd(out_norm_aid_folder)
            write.csv(apply(settings_table, 2, as.character), file = "normalization_settings.csv")
            



            if (interactive() && norm_mode == "percentile") {
                answer <- readline(paste0("Are you satisfied with automatic settings?\n",
                                "If not, change settings table in\n",
                                "Cytomata_data/<project_folder>/output/normalization/<anchor_id>/\n",
                                "please type \"continue\" when you are ready\n
                                If you want to skip normalization for a given channel, set percentile to 0\n"))
                } else {
                answer <- "continue"
            }

            if (answer != "continue") {
                cat("It seems that your typing was inprecise, we will select \"continue\" for you")
                answer <- "continue"
            }


            if (answer == "continue") {

                cat(paste0("Continuing with settings from ", a_id, "/normalization_settings.csv\n"))
                setwd(out_norm_aid_folder)
                settings_table <- read.csv("normalization_settings.csv", row.names = 1)

                cat(paste0("Reading in anchors for final scaling factor computing\n"))
                #SELECTING PRE-NORMALIZED OPTIMAL ANCHOR
                setwd(norm_folder)
                anchor_file <- dir()[grepl(pre_norm_anchor, dir())]
                input <- target_anchors$fcs[target_anchors$fcs == anchor_file]
                if (length(input) == 1) {
                    #settings for transformation
                    asinh_transform <- FALSE
                    cofac <- 1
                    exprs_set_prenorm <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
                } else {
                    stop("PRE-NORMALIZED ANCHOR NOT FOUND! Did you run first anchor? Check metafile!\n")
                }

                setwd(debar_folder)
                input <- target_anchors$fcs[target_anchors$fcs %in% dir()]
                #DROP NON-PRE-NORMALIZED ANCHOR`S BATCH
                input <- input[!grepl(pre_norm_batch, input)]
                #settings for transformation
                asinh_transform <- FALSE
                cofac <- 1
                exprs_set <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
                
                exprs_set <- rbind(exprs_set_prenorm, exprs_set)

                #compute quantile values for each channel and anchor sample
                quantiles_table_long <- quantile_table()

                cat("Computing scaling factors\n")
                setwd(out_norm_tables_folder)
                optimal_anchor <- read.csv("optimal_anchor_local.csv", row.names = 1)

                scaling_factors <- c()
                filtered_factors <- c()
                for (channel in feature_markers) {
                    temp_quantiles <- quantiles_table_long[quantiles_table_long$channel == channel, ]
                    temp_local_optimal_anchor <- optimal_anchor[optimal_anchor$channel == channel, ]

                    temp_scaling_factors <- cbind(temp_quantiles[, c(1:2)], 
                                                apply(temp_quantiles[, -c(1:2)], MARGIN = 2, FUN = function(x) as.numeric(x[temp_quantiles$a_sample == temp_local_optimal_anchor$best_anchor]) / as.numeric(x)))
                    scaling_factors <- rbind(scaling_factors, temp_scaling_factors)

                    temp_filtered_factors <- temp_scaling_factors
                    temp_filtered_factors$percentile <- settings_table[settings_table$channel == channel, "percentile"]

                    #if percentile is 0, set factor to 1 and skip adjusting the channel
                    if (settings_table[settings_table$channel == channel, "percentile"] == 0) {
                        temp_filtered_factors$factor <- 1
                    } else {
                        temp_filtered_factors$factor <- temp_filtered_factors[,settings_table[settings_table$channel == channel, "percentile"]]
                    }

                    temp_filtered_factors <- temp_filtered_factors[, c("channel", "a_sample", "percentile", "factor")]
                    filtered_factors <- rbind(filtered_factors, temp_filtered_factors)

                }
                setwd(out_norm_tables_folder)
                write.csv(scaling_factors, file = "scaling_factors.csv")

                setwd(out_norm_tables_folder)
                write.csv(filtered_factors, file = "filtered_scaling_factors.csv")

                scaling_factors_barplots()
                

                #normalizing batches
                
                normalize_batches()
                


            }
        }
    }

} else {
    stop("Normalization mode not recognized. Please check settings file.\n")
}
