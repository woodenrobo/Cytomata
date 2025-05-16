Cytomata is an open-source script collection that integrates tools and methods used for mass cytometry (CyTOF) analysis into an automated pipeline.
Author: Lev Petrov

## GUIDE
- PLEASE USE WSL, Ubuntu and VSCode, save your nerves:
  - Install WSL2: https://learn.microsoft.com/en-us/windows/wsl/install
  - Install Terminal (optional): https://github.com/microsoft/terminal
  - WSL and VSCode: https://code.visualstudio.com/docs/remote/wsl
  - R in VSCode: https://code.visualstudio.com/docs/languages/r
  - clone the repo, set the path to the repo folder in master.R (line 15)
  - run the master.R script. If doing so on a new install, check for errors for some packages, they may require additional Ubuntu libraries.
    - ```sudo apt update```
    - ```sudo apt install package_name```

- If you want to use Windows, installing all required packages will be annoying:
  - install R and Rstudio on your machine https://posit.co/download/rstudio-desktop/
  - clone the repo, set the path to the repo folder in master.R (line 15)
  - run the master.R script. If doing so on a new install, check for errors for some packages.

Changing path in master.R:

``
path_to_cytomata <- "/your/path/to/Cytomata/"
``
## From documentation in master.R

Create a folder for Cytomata data and results. The folder structure will be created upon the first run, and you will need to populate the fcs and meta folders before proceeding.
- *Always preserve the folder structure!*

```
cytomata_data_folder/
├─ project_name/
│  ├─ fcs/
│  │  ├─ 1_raw/
│  │  ├─ 2_debarcoded/
│  │  │  ├─ sample_file_1.fcs
│  │  │  ├─ sample_file_2.fcs
│  ├─ meta/
│  │  ├─ metafile.xlsx

```
metafile MUST have following columns: "id", "fcs", "batch", "analysis" and "group". script supports having multiple grouping-columns (e.g. "group_2", "group_3" etc.)

analysis column defines whether sample should be included in final analysis. duplicates of anchor/reference files are filtered out by default during analysis

analysis column also accepts values of 0 for samples to be dropped from final analysis or 2 for samples added after the main analysis run (if previous clustering needs to be preserved)

## READ SETTINGS.XLSX NOTES CAREFULLY! SOME FILES NEED TO BE CHANGED IF YOU SELECT SOME SETTINGS!


### CLEANING AND DEBARCODING
sadly, this part is best done in a cloud-based solution like OMIQ or Cytobank due to the massive data and the fact that gating is still best done manually

upload your raw data, remove calibration beads, gate on DNA channels and assign barcode identities

also a good place to MAKE SURE ALL CHANNELS HAVE THE SAME NAME BETWEEN BATCHES

export and put the files into Cytomata_data/<project_name>/fcs/2_debarcoded/. folder


### SUBSETTING OF POPULATIONS OF INTEREST
sadly, this part is best done in a cloud-based solution like OMIQ or Cytobank due to the massive data and the fact that gating is still best done manually

upload your normalized data and use a gating strategy based on your panel design to separate the cells into populations of interest

export resulting files and put them into Cytomata_data/<project_name>/fcs/4_subsets/. folder






