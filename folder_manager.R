#prepares folder structure for each new project

project_folder <- paste0(path_to_data_folder, project_name, "/")
if (!dir.exists(project_folder)) {
    cat("Folders for project    ||", project_name, "||    were successfully created\n", sep = "\t")
}
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
ifelse(!dir.exists(log_folder), dir.create(log_folder), FALSE)


output_analysis <- paste0(output_folder, "analysis", "/")
ifelse(!dir.exists(output_analysis), dir.create(output_analysis), FALSE)


if (dir.exists(project_folder) && dir.exists(fcs_folder) && dir.exists(raw_folder) && dir.exists(debar_folder) &&
     dir.exists(norm_folder) && dir.exists(subset_folder) && dir.exists(meta_folder) && dir.exists(output_folder) &&
     dir.exists(log_folder) && dir.exists(output_analysis)) {
    cat("Paths successfully set\n", sep = "")
}


ifelse(length(dir(meta_folder)) == 0, stop("PLEASE ADD METAFILE TO META FOLDER BEFORE CONTINUING\n"), TRUE)

ifelse(length(dir(debar_folder)) == 0 && do_normalization == 1, stop("PLEASE POPULATE THE DEBARCODED FCS FOLDER WITH DATA BEFORE CONTINUING\n"), TRUE)

ifelse(length(dir(norm_folder)) == 0 && do_normalization == 0 && do_database_injection == 1, stop("PLEASE POPULATE THE NORMALIZED FCS FOLDER WITH DATA BEFORE CONTINUING\n"), TRUE)

ifelse(length(dir(subset_folder)) == 0 && do_normalization == 0 && do_database_injection == 0 && do_analysis == 1, stop("PLEASE POPULATE THE PREGATED SUBSET FCS FOLDER WITH DATA BEFORE CONTINUING\n"), TRUE)


