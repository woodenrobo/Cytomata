# Description: This script installs all the necessary R packages for the Cytomata pipeline. 
# It is sourced in the master script to ensure that all the necessary packages are installed before running the pipeline.
## R Packages setup
#From CRAN:
cran_packages <- c("arrow", "scales", "ggplot2", "dplyr", "ggridges", "RColorBrewer", "MASS", "Rtsne", "kohonen",
                 "miscTools", "gplots", "Radviz", "igraph", "statmod", "devtools", "uwot", "cowplot", "limma",
                 "ggrepel", "circlize", "readxl", "ggpubr", "ggsignif", "ggfortify", "ggrastr", "Polychrome", "progress",
                 "ggnewscale", "BiocManager", "rstatix", "raster", "graph", "Hmisc",
                 "provenance", "sp", "ks", "harmony",
                 "shiny", "shinythemes", "shinytest", "shinyFiles", "Cairo", "shinyjs", "base64enc", "bsplus",
                 "shinytest2", "testthat", "plotly",
                 "parallel", "doParallel", "foreach",
                 "r2d3",
                 "scattermore")

using(cran_packages)

#From BioConductor:

bioc_packages <- c("flowCore", "flowWorkspace", "ConsensusClusterPlus", "cydar", "ncdfFlow", "edgeR",
                  "ComplexHeatmap", "limma", "M3C")

using_bioconductor(bioc_packages)

#From source:
# github_packages <- c("nolanlab/cytofCore", "JinmiaoChenLab/cytofkit", "cytolab/mem",
#                      "KChen-lab/CytoSpill", "vqv/ggbiplot", "VPetukhov/ggrastr",
#                      "rstudio/leaflet", "trafficonese/leaflet.extras")

github_packages <- c("nolanlab/cytofCore", "JinmiaoChenLab/cytofkit", "cytolab/mem",
                     "KChen-lab/CytoSpill", "vqv/ggbiplot", "VPetukhov/ggrastr")


using_github(github_packages)



cat("All packages installed, you are good to go!\n")
