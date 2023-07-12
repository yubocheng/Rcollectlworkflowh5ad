# Docker inheritance
FROM bioconductor/bioconductor_docker:devel
ENV knitr_eval FALSE
ENV fileId "e2874fd9-af11-4adf-a47a-f380b7606434"
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
COPY install_Rcollectl.R /tmp/install_Rcollectl.R
COPY run_Rcollectl.R /tmp/run_Rcollectl.R

RUN chmod +x /tmp/install_Rcollectl.R
RUN chmod +x /tmp/run_Rcollectl.R

RUN /tmp/install_Rcollectl.R

WORKDIR /output

CMD /tmp/run_Rcollectl.R $knitr_eval $fileId $sample $dgCMatrix $core $mem_gb
