# Description: This script installs all the necessary R packages for the Cytomata pipeline. 
# It is sourced in the master script to ensure that all the necessary packages are installed before running the pipeline.
## R Packages setup
#From CRAN:
cran_packages <- c("arrow", "scales", "ggplot2", "dplyr", "ggridges", "RColorBrewer", "MASS", "Rtsne", "kohonen",
                 "miscTools", "gplots", "Radviz", "igraph", "statmod", "devtools", "uwot", "cowplot", "limma",
                 "ggrepel", "circlize", "readxl", "ggpubr", "ggsignif", "ggfortify", "Polychrome", "progress",
                 "ggnewscale", "BiocManager", "rstatix", "raster",
                 "provenance", "sp", "ks",
                 "shiny", "shinythemes", "shinyFiles", "Cairo", "shinyjs", "base64enc", 
                 "shinytest2", "testthat", "plotly",
                 "parallel", "doParallel", "foreach")

using(cran_packages)

#From BioConductor:

bioc_packages <- c("flowCore", "flowWorkspace", "ConsensusClusterPlus", "cydar", "ncdfFlow", "edgeR",
                  "ComplexHeatmap", "limma", "M3C")

using_bioconductor(bioc_packages)

#From source:
github_packages <- c("nolanlab/cytofCore", "JinmiaoChenLab/cytofkit", "cytolab/mem",
                     "KChen-lab/CytoSpill", "vqv/ggbiplot", "VPetukhov/ggrastr",
                     "rstudio/leaflet", "trafficonese/leaflet.extras")


using_github(github_packages)



cat("All packages installed, you are good to go!\n")
