#prepares folder structure for each new project
project_folder <- paste0(path_to_data_folder, project_name, "/")
ifelse(!dir.exists(project_folder), dir.create(project_folder), FALSE)


fcs_folder <- paste0(project_folder, "fcs", "/")
ifelse(!dir.exists(fcs_folder), dir.create(fcs_folder ), FALSE)

raw_folder <- paste0(fcs_folder , "1_raw", "/")
ifelse(!dir.exists(raw_folder), dir.create(raw_folder), FALSE)

debar_folder <- paste0(fcs_folder , "2_debarcoded", "/")
ifelse(!dir.exists(debar_folder), dir.create(debar_folder), FALSE)

norm_folder <- paste0(fcs_folder , "3_normalized", "/")
ifelse(!dir.exists(norm_folder), dir.create(norm_folder), FALSE)

subset_folder <- paste0(fcs_folder , "4_subsets", "/")
ifelse(!dir.exists(subset_folder), dir.create(subset_folder), FALSE)


meta_folder <- paste0(project_folder, "meta", "/")
ifelse(!dir.exists(meta_folder), dir.create(meta_folder), FALSE)


output_folder <- paste0(project_folder, "output", "/")
ifelse(!dir.exists(output_folder), dir.create(output_folder), FALSE)

log_folder <- paste0(output_folder, "logs", "/")
ifelse(!dir.exists(log_folder), dir.create(log_folder ), FALSE)

cat("Folders for project    ||", project_name, "||    were successfully created\n", sep = "\t")
cat("Paths successfully set", sep = "")

