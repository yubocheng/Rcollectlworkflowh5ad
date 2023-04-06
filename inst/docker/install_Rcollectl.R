#! /usr/bin/env Rscript

# Install required packages
BiocManager::install(c("ggplot2", "dplyr", "pheatmap", "sessioninfo", "devtools", "GenomicFeatures", "tidyr", "cellxgenedp", "HDF5Array", "matrixStats", "SingleCellExperiment", "ensembldb", "EnsDb.Hsapiens.v86", "scater",  "BiocParallel", "scran", "batchelor", "BiocNeighbors", "uwot", "bluster", "celldex", "SingleR", "rmarkdown", "BiocStyle"))

# Install Rcollectl
BiocManager::install("vjcitn/Rcollectl")

# Install zellkonverter
BiocManager::install("mtmorgan/zellkonverter", ref = "include-native-dimnames")

# Install workflow vignette
BiocManager::install("yubocheng/Rcollectlworkflowh5ad", build_vignettes = TRUE)
