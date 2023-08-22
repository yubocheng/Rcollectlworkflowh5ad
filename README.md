## Rcollectl Workflow

The vignettes in this package follows single-cell analysis workflow
(workflow chapters of the “Orchestrating Single-Cell Analysis with
Bioconductor” book) to analyze h5ad file from cellxgene dataset and
tracks computing resources usage with R package `Rcollectl`.

Two workflows Rcollectlworkflowh5ad and
workflow\_h5ad\_DelayedArray\_parameters are available at Dockstore,
also set up on Terra workspace,
bioconductor-rpci-yubo/Rcollectlworkflowh5ad.

Vignette terra\_data\_import.Rmd includes R code for importing data of
h5ad file(s) to Terra workspace to run the workflows. And
performance\_summary.Rmd provides a performance summary of running the
two workflows, functions summary\_result\_dataset() and
summary\_result() generate the csv files needed for this vignette.
