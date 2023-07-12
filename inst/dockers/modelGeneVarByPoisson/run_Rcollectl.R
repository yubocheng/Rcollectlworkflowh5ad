#! /usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

v = vignette(topic="modelGeneVarByPoisson", package = "Rcollectlworkflowh5ad")
path = file.path(v$Dir, "doc", "modelGeneVarByPoisson.Rmd")
file.copy(from = path, to = getwd())
rmarkdown::render("modelGeneVarByPoisson.Rmd", output_dir = ".", params = list(knitr_eval=as.logical(args[1]), fileId=args[2], sample=args[3], mem_gb=as.integer(args[4])))
