#! /usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

v = vignette(topic="workflow_cellxgene_h5ad_DelayedArray_parameters", package = "Rcollectlworkflowh5ad")
path = file.path(v$Dir, "doc", "workflow_cellxgene_h5ad_DelayedArray_parameters.Rmd")
file.copy(from = path, to = getwd())
rmarkdown::render("workflow_cellxgene_h5ad_DelayedArray_parameters.Rmd", output_dir = ".", params = list(knitr_eval=as.logical(args[1]), file_url=args[2], sample=args[3], mem_gb=as.integer(args[4])))
