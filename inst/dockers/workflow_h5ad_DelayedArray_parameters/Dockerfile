# Docker inheritance
FROM ycheng2022/bioconductor_docker_workflow_cellxgene_h5ad:devel
ENV knitr_eval FALSE
ENV file_url "https://datasets.cellxgene.cziscience.com/0c6f2c74-8d27-4c1f-b63a-a766bc9f9490.h5ad"
ENV sample "Name"
ENV mem_gb 120

# install Rcollectl with the vignette
COPY run_vignette.R /tmp/run_vignette.R

RUN chmod +x /tmp/run_vignette.R

WORKDIR /output

CMD /tmp/run_vignette.R $knitr_eval $file_url $sample $mem_gb
