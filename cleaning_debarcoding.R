library(ncdfFlow)

raw_data_path <- "~/DOCTORATE/Cytomata_data/input/raw_experiment_data/panel1/"

setwd(raw_data_path)
input <- dir(recursive = TRUE, full.names = TRUE)
input <- input[grepl(".fcs$", input)]

fs <- read.ncdfFlowSet(files=input[1], truncate_max_range = FALSE)
