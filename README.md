## Rcollectl Workflow

The vignette in this package follows single-cell analysis workflow
(workflow chapters of the “Orchestrating Single-Cell Analysis with
Bioconductor” book) to analyze h5ad file from cellxgene dataset and
tracks computing resources usage with R package `Rcollectl`.

Suggested computing resources: 60 GB memory, 3 CPU cores when parameter
knitr\_eval set to TRUE to knit the vignette. FileId is the file id of
h5ad file to analyze. Sample should be one of column names.

Build docker image with files in inst/docker:

    docker build -t bioconductor_docker_workflow_cellxgene_h5ad:devel .

Knit vignette with built docker image and generate output at
LOCAL\_DIRECTORY:

    docker run -it -v LOCAL_DIRECTORY:/output --rm \
        --env knitr_eval=TRUE \
        --env fileId="e2874fd9-af11-4adf-a47a-f380b7606434" \
        --env sample="Name" \
        --env dgCMatrix=TRUE \
        --env core=3 \
        --env mem_gb=60 \
        bioconductor_docker_workflow_cellxgene_h5ad:devel
