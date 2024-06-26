
using <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) { 
        install.packages(need)
        lapply(need, require, character.only = TRUE)
    }
}

using_bioconductor <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) { 
        BiocManager::install(need)
        lapply(need, require, character.only = TRUE)
    }
}

using_github <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) { 
        devtools::install_github(need)
        lapply(need, require, character.only = TRUE)
    }
}

parse_settings <- function() {
    library(readxl)
    library(stringr)
    #looking for settings.xlsx in Cytomata root folder
    settings <- read_xlsx("settings.xlsx")
}

load_metafile <- function(meta_naming_scheme) {
    library(readxl)
    #looking for metafile in meta folder
    metafile <- dir(meta_folder)[grepl(meta_naming_scheme, dir(meta_folder))]
    meta <- read_xlsx(paste0(meta_folder, metafile))
    required_columns <- c("id", "fcs")
    if (sum(required_columns %in% colnames(meta)) == length(required_columns)){
        return(meta)
    } else {
        stop("CHECK YOUR METAFILE!\nMetafile MUST have following columns: \"id\", \"fcs\"\n")
    }
}


load_panel <- function(...) {
    if (sum(grepl("panel.csv", dir(meta_folder)))>0) {
        p <- read.csv(paste0(meta_folder, "panel.csv"))
        cat("Panel restored from .csv\n")
        return(p)
    } else {
        #all features need to have the same name across the samples
        setwd(debar_folder)
        fcs <- dir(debar_folder)[1]
        fs <- flowCore::read.FCS(fcs, which.lines=1:100, transformation=FALSE, truncate_max_range=FALSE)
        p <- NULL
        p$fcs_colname <- fs@parameters@data[,"name"]
        p$antigen <- fs@parameters@data[,"desc"]
        p <- as.data.frame(p)
        p <- p[stringr::str_detect(p$fcs_colname, pattern = "[1-9]"),]
        p$antigen <- stringr::str_split(p$antigen, pattern = "_", n = 2) %>% sapply(function(x) x[2])
        #remove pre-processing channels (pre-processing has already been done in OMIQ)
        p$feature <- 1
        p[is.na(p$antigen),  "feature"] <- 0
        p[grepl("B2M|DNA|Bead|LD|Live|Dead|ID|Cell-ID|Cell_ID", p$antigen, perl = TRUE), "feature"] <- 0
        setwd(meta_folder)
        write.csv(p, "panel.csv", row.names = FALSE)
        cat("Panel extracted from .fcs, saved as .csv in project's /meta folder\n")
        cat("Please set \"feature\" variable to 0 for empty or unused channels\n")
        return(p)
    }

}


check_sampling_rate_changes <- function() {
    if (sum(grepl(paste0("first_run_sampling_rate"), dir(output_data_sub)) == TRUE) > 0) {
        previous_sampling_rate <- as.numeric(read.csv(paste0(output_data_sub, "first_run_sampling_rate.csv"))[-1])
        if (sampling_rate != previous_sampling_rate) {
            cat("Sampling rate has changed since the first run!\n",
            "Restoring previous clustering and UMAP results will not be possible!\n")

            if (interactive()) {
                answer <- readline(paste0("Do you wish to proceed? New clustering and UMAP will be calculated!\n",
                                "If yes, type \"continue\"\n",
                                "If not, type \"backup\" to automatically set the old sampling_rate\n"))
            } else {
                answer <- "continue"
                cat("\n\n****************************************************\n",
                    "ATTENTION! NEW CLUSTERING AND UMAP WILL BE CALCULATED!\n",
                    "STOP NOW IF YOU DO NOT WISH TO OVERWRITE THE RESULTS!",
                    "\n****************************************************\n\n")
            }

            if (answer == "backup") {
                sampling_rate <- previous_sampling_rate
                cat("You have chosen to reset back to the sampling rate of", sampling_rate, "\n")
            } else if (answer == "continue") {
                cat("Continuing with new sampling rate of", sampling_rate, "\n")
            } else {
                cat("It seems you have typed an incorrect answer!\n")
                check_sampling_rate_changes()
            }
        }
    }

}


inject_fcs <- function(input, filter_features, asinh_transform, cofac, sampling_rate = 1, silent = FALSE, event_cutoff = 0) {
    library(flowCore)
    library(progress)
    library(dplyr)
    cat('INJECTING DATA\n')

    if (silent != TRUE) {
    pb <- progress_bar$new(format = "Injecting data\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                        total = length(input),
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = FALSE,    # If TRUE, clears the bar when finish
                        width = 110)      # Width of the progress bar
    }

    exprs_set <- data.frame()
    sample <- c()
    total_events <- 0
    if (silent != TRUE) {
        cat("Feature markers selected are:\n")
        cat(feature_markers, "\n", sep=" ")
        pb$tick(0)
    }

    
    for (f in input) {
        fcs <- read.FCS(filename = f, transformation = FALSE, truncate_max_range = FALSE)
        fcs_channel_desc <<- as.vector(fcs@parameters@data$desc)
        fcs_channel_name <<- as.vector(fcs@parameters@data$name)
        exprs <- as.data.frame(fcs@exprs)
        cat(nrow(exprs), "events in", rep(basename(f)), "\n")
        markers <- gsub(pattern = "^.*?_", replacement = "", x = as.vector(fcs@parameters@data$desc))
        colnames(exprs)[which(!is.na(markers))] <- markers[which(!is.na(markers))]
        if (filter_features == TRUE) {
            exprs <- exprs[, colnames(exprs) %in% feature_markers]
        }

        exprs_set <- rbind(exprs_set, exprs)
        sample <- append(sample, rep(basename(f), nrow(exprs)))
        total_events <- total_events + nrow(exprs)

        if (silent != TRUE) {
            pb$tick()
        }
    }
    exprs_set$sample <- sample
    cat(total_events, "total events in the read expression matrix\n")

    sample_counts <- as.data.frame(table(exprs_set$sample))
    colnames(sample_counts) <- c("sample", "freq")

    if (event_cutoff == 0) {
        cat("Event cutoff of 0 selected, filtering of small samples omitted\n")
        cat("Samples that are too small may skew cluster abundances!\n")
    } else {
        cat("Event cutoff of", event_cutoff, "applied, filtering out small samples\n")
        small_samples <- sample_counts$sample[sample_counts$freq < event_cutoff]
        exprs_set <- exprs_set[!exprs_set$sample %in% small_samples, ]
        cat(length(small_samples), "samples were filtered out!\n")
        print(small_samples)
        sample_counts <- as.data.frame(table(exprs_set$sample))
        colnames(sample_counts) <- c("sample", "freq")
    }

    if (asinh_transform == TRUE) {
        #transform the expression values
        exprs_set[, colnames(exprs_set) %in% feature_markers] <- asinh(exprs_set[, colnames(exprs_set) %in% feature_markers] / cofac)
        cat("Applied arcsinh transformation with the cofactor of", cofac, "\n")
    }


    if (sampling_rate < 1) {
        cat("\n==========\nRANDOM DOWNSAMPLING INITIATED\n==========\n")
        cat("Applied downsampling factor of", sampling_rate, "\n")

        absolute_cutoff <- ceiling(mean(sample_counts$freq))
        cat("Mean number of events is", absolute_cutoff, "and will be used as lower cutoff\n")

        downsampling_index <- sample_counts[!sample_counts$freq < absolute_cutoff, ]

        pb <- progress_bar$new(format = "Downsampling\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(input),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = FALSE,    # If TRUE, clears the bar when finish
                    width = 110)      # Width of the progress bar
        
        temp_set <- c()
        for (s in sample_counts$sample) {
            if (s %in% downsampling_index$sample) {
                target_event_number <- downsampling_index[downsampling_index$sample == s, "freq"] * sampling_rate
                if (target_event_number < absolute_cutoff) {
                    target_event_number <- absolute_cutoff
                } 
                exprs_sample <- exprs_set[exprs_set$sample == s, ]
                exprs_sample <- exprs_sample[sample(length(exprs_sample), size = target_event_number, replace = FALSE), ]
                temp_set <- rbind(temp_set, exprs_sample)
            } else {
                exprs_sample <- exprs_set[exprs_set$sample == s, ]
                temp_set <- rbind(temp_set, exprs_sample)
            }
        pb$tick()
        }
        exprs_set <- temp_set

        rm(exprs_sample, temp_set)
        gc()
    }
    if (sampling_rate > 1) {
        cat("\n==========\nRANDOM UPSAMPLING INITIATED\n==========\n")
        cat("Applied oversampling factor of", sampling_rate, "\n")

        
        absolute_cutoff <- ceiling(mean(sample_counts$freq))
        cat("Mean number of events is", absolute_cutoff, "and will be used as upper cutoff\n")

        oversampling_index <- sample_counts[!sample_counts$freq > absolute_cutoff, ]

        pb <- progress_bar$new(format = "Upsampling\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                    total = length(input),
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = FALSE,    # If TRUE, clears the bar when finish
                    width = 110)      # Width of the progress bar
        
        temp_set <- c()
        for (s in sample_counts$sample) {
            if (s %in% oversampling_index$sample) {
                starting_event_number <- oversampling_index[oversampling_index$sample == s, "freq"]
                target_event_number <- oversampling_index[oversampling_index$sample == s, "freq"] * sampling_rate 
                added_event_number <- target_event_number - starting_event_number
                if (target_event_number > absolute_cutoff) {
                    target_event_number <- absolute_cutoff
                    added_event_number <- target_event_number - starting_event_number
                } 
                exprs_sample <- exprs_set[exprs_set$sample == s, ]
                exprs_sample_resampled <- exprs_sample[sample(length(exprs_sample), size = added_event_number, replace = TRUE), ]
                exprs_sample$resampled <- "no"
                exprs_sample_resampled$resampled <- "yes"
                exprs_sample <- rbind(exprs_sample, exprs_sample_resampled)
                temp_set <- rbind(temp_set, exprs_sample)
            } else {
                exprs_sample <- exprs_set[exprs_set$sample == s, ]
                exprs_sample$resampled <- "no"
                temp_set <- rbind(temp_set, exprs_sample)
            }
        pb$tick()
        }
        exprs_set <- temp_set
        rm(exprs_sample, temp_set)
        gc()
    }


    sample_counts <<- sample_counts
    return(exprs_set)
}
