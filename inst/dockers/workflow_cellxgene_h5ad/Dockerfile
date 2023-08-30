# Docker inheritance
FROM bioconductor/bioconductor_docker:devel
ENV knitr_eval FALSE
ENV file_url "https://datasets.cellxgene.cziscience.com/0c6f2c74-8d27-4c1f-b63a-a766bc9f9490.h5ad"
ENV sample "Name"
ENV dgCMatrix TRUE
ENV core 3
ENV mem_gb 60

# download and install collectl
RUN wget "https://sourceforge.net/projects/collectl/files/latest/download" \
	&& tar -xvzf download \
	&& cd collectl-* \
	&& ./INSTALL \
	&& cd .. \
	&& rm -rf download \
	&& rm -rf collectl-* 

# install Rcollectl with the vignette
COPY install_dependencies.R /tmp/install_dependencies.R
COPY install_package.R /tmp/install_package.R
COPY run_vignette.R /tmp/run_vignette.R

RUN chmod +x /tmp/install_dependencies.R
RUN chmod +x /tmp/install_package.R
RUN chmod +x /tmp/run_vignette.R

RUN /tmp/install_dependencies.R
RUN /tmp/install_package.R

WORKDIR /output

CMD /tmp/run_vignette.R $knitr_eval $file_url $sample $dgCMatrix $core $mem_gb
