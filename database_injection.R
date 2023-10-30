install.packages("arrow")
library(arrow)
library(dplyr)

parquet_out_path <- paste0(norm_folder, "parquet/")

setwd(norm_folder)
input <- dir(recursive = TRUE, full.names = TRUE)
input <- input[grepl(".fcs$", input)]

ifelse(!dir.exists(parquet_out_path), dir.create(parquet_out_path), FALSE)

#meta-based filtering out of duplicate anchors here to select input
input <- meta[!meta$id %in% dir(norm_folder) & duplicated(target_anchors$id)]

for (f in input) {
  # fcs <- flowCore::read.FCS(filename = f, transformation = FALSE, truncate_max_range = FALSE)
  
  downsampling_rate <- 1
  #settings for transformation
  asinh_transform <- TRUE
  cofac <- 1
  exprs_set <- inject_fcs(f, filter_features = TRUE, asinh_transform = asinh_transform, cofac = cofac)
  parquet_filename <- gsub("./|.fcs", "", f)
  arrow::write_parquet(as.data.frame(exprs_set), paste0(parquet_out_path, parquet_filename, ".parquet"))
  rm(exprs_set)
  gc()
}

setwd(norm_folder)
ds <- arrow::open_dataset("parquet")

means <- ds %>%
          summarize(across(all_of(feature_markers), ~ mean(.))) %>%
                collect() %>% as.data.frame() %>% tidyr::pivot_longer(cols = everything()) 
                          
stdev <- ds %>%
          summarize(across(all_of(feature_markers), ~ sd(.))) %>%
                collect() %>% as.data.frame() %>% tidyr::pivot_longer(cols = everything())

meansd <- cbind(means, stdev[,2]) 
colnames(meansd) <- c("Channel", "Mean", "SD")

setwd(output_folder)
write.csv(meansd, file = "meansd.csv", row.names=TRUE)

