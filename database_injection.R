install.packages("arrow")
library(arrow)
library(dplyr)


parquet_out_path <- paste0(raw_data_path, "parquet/")


setwd(raw_data_path)
input <- dir(recursive = TRUE, full.names = TRUE)
input <- input[grepl(".fcs$", input)]


ifelse(!dir.exists(parquet_out_path), dir.create(parquet_out_path), FALSE)

for (f in input) {
  fcs <- flowCore::read.FCS(filename = f, transformation = FALSE, truncate_max_range = FALSE)
  parquet_filename <- gsub("./|.fcs", "", f)
  arrow::write_parquet(as.data.frame(fcs@exprs), paste0(parquet_out_path, parquet_filename, ".parquet"))
  rm(fcs)
  gc()
}

setwd(raw_data_path)
ds <- arrow::open_dataset("parquet")




vars <- names(ds)

ds %>%
    summarize(across(all_of(vars), ~ mean(.))) %>%
    collect()
    


