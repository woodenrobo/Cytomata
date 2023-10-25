cat(paste0("\n==========\nDIAGNOSTICS MODULE STARTED\n=========="))
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

    out_norm_diag_folder <- paste0(out_norm_folder, "diagnostics", "/")
    ifelse(!dir.exists(out_norm_diag_folder), dir.create(out_norm_diag_folder), FALSE)

    out_norm_aid_diag_folder <- paste0(out_norm_diag_folder, a_id, "/")
    ifelse(!dir.exists(out_norm_aid_diag_folder), dir.create(out_norm_aid_diag_folder), FALSE)

    out_norm_tables_diag_folder <- paste0(out_norm_aid_diag_folder, "tables", "/")
    ifelse(!dir.exists(out_norm_tables_diag_folder), dir.create(out_norm_tables_diag_folder), FALSE)

    out_norm_dens_diag_folder <- paste0(out_norm_aid_diag_folder, "densities", "/")
    ifelse(!dir.exists(out_norm_dens_diag_folder), dir.create(out_norm_dens_diag_folder), FALSE)



    target_anchors <- meta[meta$id == a_id, ]
    cat(paste0("\n ANCHOR SAMPLE SELECTED IS: ", a_id, "\n"))
    cat(paste0("\n BATCHES PRESENT IN NON-NORMALIZED INPUT ARE: \n"))
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
    exprs_set_before <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
    exprs_set_before$norm_state <- "BEFORE"
    
    before_means <- channel_mean_compute(data_set = exprs_set_before)
    before_means$norm_state <- "BEFORE"


    cat(paste0("\n BATCHES PRESENT IN NORMALIZED INPUT ARE: \n"))
    setwd(norm_folder)
    print(unique(target_anchors$batch[target_anchors$fcs %in% dir()]))
    input <- target_anchors$fcs[target_anchors$fcs %in% dir()]
    downsampling_rate <- 1
    #settings for transformation
    asinh_transform <- FALSE
    cofac <- 1
    exprs_set_after <- inject_fcs(input, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
    exprs_set_after$norm_state <- "AFTER"

    after_means <- channel_mean_compute(data_set = exprs_set_after)
    after_means$norm_state <- "AFTER"

    before_after_means <- rbind(before_means, after_means)

    exprs_set <- rbind(exprs_set_before, exprs_set_after)
    rm(exprs_set_before, exprs_set_after, before_means, after_means)
    gc()

    setwd(out_norm_tables_folder)
    optimal_anchor <- read.csv("optimal_anchor_local.csv", row.names = 1)
    optimal_percentile <- read.csv("optimal_percentiles_table.csv", row.names = 1)

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
            diagnostics_ridges_wo_zeroes()
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
            pb$tick()
            diagnostics_ridges()
        }
    }


    } else {

    }




}
