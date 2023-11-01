
using <- function(...) {
    libs < -unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) { 
        install.packages(need)
        lapply(need, require, character.only = TRUE)
    }
}

using_bioconductor <- function(...) {
    libs < -unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) { 
        BiocManager::install(need)
        lapply(need, require, character.only = TRUE)
    }
}

using_github <- function(...) {
    libs < -unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) { 
        devtools::install_github(need)
        lapply(need, require, character.only = TRUE)
    }
}


load_metafile <- function(meta_naming_scheme) {
    library(readxl)
    #looking for metafile in meta folder
    metafile <- dir(meta_folder)[grepl(meta_naming_scheme, dir(meta_folder))]
    meta <- read_xlsx(paste0(meta_folder, metafile))
    required_columns <- c("id", "fcs", "group")
    if (sum(required_columns %in% colnames(meta)) == length(required_columns)){
        return(meta)
    } else {
        stop("CHECK YOUR METAFILE!\nMetafile MUST have following columns: \"id\", \"fcs\", \"group\". \nScrip'panekt supports having multiple grouping-columns (e.g. \"group_2\", \"group_3\" etc.)")
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
        p$antigen <- stringr::str_split(p$antigen, pattern = "_") %>% sapply(function(x) x[2])
        #remove pre-processing channels (pre-processing has already been done in OMIQ)
        p$feature <- 1
        p[is.na(p$antigen),  "feature"] <- 0
        p[grepl("B2M|DNA|Bead|LD|Live|Dead|ID|Cell-ID", p$antigen, perl = TRUE), "feature"] <- 0
        setwd(meta_folder)
        write.csv(p, "panel.csv", row.names = FALSE)
        cat("Panel extracted from .fcs, saved as .csv in project's /meta folder\n")
        cat("Please set \"feature\" variable to 0 for empty or unused channels\n")
        return(p)
    }

}


inject_fcs <- function(input, filter_features, asinh_transform, cofac){
    library(flowCore)
    library(progress)
    library(dplyr)
    cat('INJECTING DATA\n')

    pb <- progress_bar$new(format = "Injecting data\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                        total = length(input),
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = FALSE,    # If TRUE, clears the bar when finish
                        width = 120)      # Width of the progress bar

    exprs_set <- data.frame()
    sample <- c()
    total_events <- 0
    cat("Feature markers selected are:\n")
    cat(feature_markers, "\n", sep=" ")
    pb$tick(0)
    for (f in input) {
        fcs <- read.FCS(filename = f, transformation = FALSE, truncate_max_range = FALSE)
        fcs_channel_desc <<- as.vector(fcs@parameters@data$desc)
        fcs_channel_name <<- as.vector(fcs@parameters@data$name)
        exprs <- as.data.frame(fcs@exprs)
        cat(nrow(exprs), "events in", rep(basename(f)), "\n")
        markers <- gsub(pattern = ".*_", replacement = "", x = as.vector(fcs@parameters@data$desc))
        colnames(exprs)[which(!is.na(markers))] <- markers[which(!is.na(markers))]
        if (filter_features == TRUE) {
            exprs <- exprs[, colnames(exprs) %in% feature_markers]
        }
        if (downsampling_rate < 1) {
            cat('\n==========\nApplied downsampling rate of ', downsampling_rate, '\n==========\n')
            cat("Sampled", round(nrow(exprs) * downsampling_rate), "events ", "\n\n")
            exprs_sample <- exprs[sample(round(nrow(exprs) * downsampling_rate), replace = FALSE), ]
            exprs_set <- rbind(exprs_set, exprs_sample)
            sample <- append(sample, rep(basename(f), round(nrow(exprs) * downsampling_rate)))
            total_events <- total_events + round(nrow(exprs) * downsampling_rate)
        } else {
            exprs_set <- rbind(exprs_set, exprs)
            sample <- append(sample, rep(basename(f), nrow(exprs)))
            total_events <- total_events + nrow(exprs)
        }
        pb$tick()
    }
    exprs_set$sample <- sample
    cat(total_events, "total events in the read expression matrix\n")

    if (asinh_transform == TRUE) {
        #transform the expression values
        exprs_set[, colnames(exprs_set) %in% feature_markers] <- asinh(exprs_set[, colnames(exprs_set) %in% feature_markers] / cofac)
        cat("Applied arcsinh transformation with the cofactor of", cofac, "\n")
    }

    return(exprs_set)
}
