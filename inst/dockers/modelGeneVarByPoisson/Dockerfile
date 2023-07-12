# Docker inheritance
FROM ycheng2022/bioconductor_docker_workflow_cellxgene_h5ad:devel
ENV knitr_eval FALSE
ENV fileId "e2874fd9-af11-4adf-a47a-f380b7606434"
ENV sample "Name"
ENV mem_gb 120

# install Rcollectl with the vignette
COPY run_Rcollectl.R /tmp/run_Rcollectl.R

RUN chmod +x /tmp/run_Rcollectl.R

WORKDIR /output

CMD /tmp/run_Rcollectl.R $knitr_eval $fileId $sample $mem_gb
