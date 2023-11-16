library(arrow)
library(dplyr)

parquet_out_path <- paste0(norm_folder, "parquet/")

setwd(norm_folder)
input <- dir(recursive = TRUE, full.names = TRUE)
input <- input[grepl(".fcs$", input)]

ifelse(!dir.exists(parquet_out_path), dir.create(parquet_out_path), FALSE)

#meta-based filtering out of duplicate anchors here to select input
duplicated_anchors <- unlist(meta[meta$id %in% anchor_ids & duplicated(meta$id), "fcs"])
cat("\n", length(duplicated_anchors), "anchor samples detected in meta, they will be omitted\n")

input_duplicated_anchors <- input[input %in% duplicated_anchors]
cat("\n", length(input_duplicated_anchors), "anchor samples detected in folder, they will be omitted\n")

filtered_meta <- meta[!meta$fcs %in% duplicated_anchors,]
input <- unlist(filtered_meta[filtered_meta$fcs %in% dir(norm_folder), "fcs"])



if (length(input) < length(filtered_meta$fcs)) {
    warning(paste0("Only ", length(input), " out of ", length(filtered_meta$fcs), " samples in meta are present in input directory"))
}
if (length(input) > length(filtered_meta$fcs)) {
    warning(paste0(length(input), " out of ", length(filtered_meta$fcs), " samples in meta are present in input directory"))
}


pb <- progress_bar$new(format = "Converting to Parquet\n(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]\n",
                        total = length(input),
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = FALSE,    # If TRUE, clears the bar when finish
                        width = 120)      # Width of the progress bar
pb$tick(0)
for (f in input) {  
  sampling_rate <- 1
  #settings for transformation
  asinh_transform <- TRUE
  cofac <- 1
  exprs_set <- inject_fcs(f, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
  parquet_filename <- gsub("./|.fcs", "", f)
  arrow::write_parquet(as.data.frame(exprs_set), paste0(parquet_out_path, parquet_filename, ".parquet"))
  rm(exprs_set)
  gc()
  pb$tick()
}

setwd(norm_folder)
ds <- arrow::open_dataset("parquet")

means <- ds %>%
          summarize(across(all_of(feature_markers), ~ mean(.))) %>%
                collect() %>%
                as.data.frame() %>%
                tidyr::pivot_longer(cols = everything())
colnames(means) <- c("channel", "mean") 
                          
stdev <- ds %>%
          summarize(across(all_of(feature_markers), ~ sd(.))) %>%
                collect() %>%
                as.data.frame() %>%
                tidyr::pivot_longer(cols = everything())
colnames(stdev) <- c("channel", "stdev") 

meansd <- left_join(means, stdev, by = "channel") 

setwd(meta_folder)
write.csv(meansd, file = "meansd.csv", row.names = FALSE)

