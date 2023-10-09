


#NOT UPDATED FOR NOW, JUST COLLECTING NEEDED LIBRARIES IN EACH SCRIPT AT THE TOP IN library() CALLS
#WILL PUT THEM ALL HERE AFTERWARDS
#ALSO HAVE TO CHECK THE BEST ORDER

## R Packages setup
#From CRAN:
cran_packages <- c("scales", "ggplot2", "dplyr", "ggridges", "RColorBrewer", "MASS", "Rtsne", "kohonen",
                 "miscTools", "gplots", "Radviz", "igraph", "statmod", "devtools", "uwot", "cowplot", "limma",
                 "ggrepel", "circlize", "readxl", "ggpubr", "ggsignif", "ggfortify", "Polychrome", "progress", "ggnewscale", "BiocManager", "provenance")

using(cran_packages)

install.packages("duckdb")

#From BioConductor:

bioc_packages <- c("flowCore", "ConsensusClusterPlus", "cydar", "ncdfFlow", "edgeR",
                  "ComplexHeatmap", "limma")

using_bioconductor(bioc_packages)

#From source:
github_packages <- c("nolanlab/cytofCore", "JinmiaoChenLab/cytofkit", "cytolab/mem", "KChen-lab/CytoSpill", "vqv/ggbiplot")

remotes::install_github("RGLab/cytoinstaller")

using_github(github_packages)

#Cytoverse packages
cytoinstaller::install_cyto(bioc_ver = "devel")

devtools::install_github("DillonHammill/CytoExploreR")
devtools::install_github("RGLab/ncdfFlow")


cat("All packages installed, you are good to go!\n")
