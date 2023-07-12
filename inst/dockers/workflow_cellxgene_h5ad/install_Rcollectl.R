#! /usr/bin/env Rscript

# Install required packages
BiocManager::install(c("ggplot2", "dplyr", "pheatmap", "sessioninfo", "devtools", "GenomicFeatures", "cellxgenedp", "HDF5Array", "SingleCellExperiment", "EnsDb.Hsapiens.v86", "scater",  "BiocParallel", "scran", "batchelor", "BiocNeighbors", "uwot", "bluster", "celldex", "SingleR", "rmarkdown", "BiocStyle", "AnVIL", "Rcollectl", "zellkonverter"))

# Install workflow vignette
BiocManager::install("yubocheng/Rcollectlworkflowh5ad", build_vignettes = TRUE)
